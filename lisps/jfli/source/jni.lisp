;    Copyright (c) Rich Hickey. All rights reserved.
;    The use and distribution terms for this software are covered by the
;    Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;    which can be found in the file CPL.TXT at the root of this distribution.
;    By using this software in any fashion, you are agreeing to be bound by
;    the terms of this license.
;    You must not remove this notice, or any other, from this software.

#|
This is a straight wrapper around the JNI API
Originally I intended to expose this API directly, but it turns out
that JNI is very sensitive to errors, and, given bad args, wrong types etc
causes the JVM (and Lisp) to crash, not very much in the spirit of safe, robust,
interactive development offered by Lisp

So, now this just forms the substrate under jfli, which uses the Reflection API, and is much
more robust and error tolerant, at some cost in speed I guess.

Bottom line is you shouldn't be using this API directly unless you are extending jfli,
and then you must take care not to allow bad end-user data to pass through to JNI. 

Caveat emptor.

I have tried to limit LispWorks FLI code to this file.
|#

(defpackage :jni
  (:export
   :*jni-lib-path*
   :*pvm*
   :*penv*
   :register-invocation-handler
   :create-jvm
   :JNI-VERSION-1-2
   :JNI-VERSION-1-4
   :JNI-OK
   :java-ref
   :jvoid :jboolean :jbyte :jchar :jshort :jint :jlong :jfloat :jdouble :jsize
   :jobject :jclass :jthrowable :jstring :jarray
   :jboolean-array :jbyte-array :jchar-array :jshort-array :jint-array :jlong-array
   :jfloat-array :jdouble-array :jobject-array
   :jfield-id :jmethod-id :jweak
   :pvm :penv
   :jvalue
   :arg-array
   :jni-native-method :jni-env
   :java-vm :java-vm-option :jdk-1-1-init-args
   :jni-get-default-java-vm-init-args :java-vm-inits-args
   :jni-create-java-vm :jni-get-created-java-vms
   :try :try-null :try-neg
   :local-ref-to-global-ref :local-ref-to-string
   :def-jni-function :def-jni-functions :def-jni-constructor :def-jni-field
   :jaref :convert-to-java-string :convert-from-java-string :java-ref-p
   :is-name-of-primitive :split-package-and-class))

(in-package :jni)

(defvar *jni-lib-path*
#+:MACOSX "/System/Library/Frameworks/JavaVM.framework/JavaVM"
#+:WIN32 "C:/j2sdk1.4.2_01/jre/bin/client/jvm.dll"
"Set this to point to your jvm dll prior to calling create-jvm")

(defparameter *pvm* nil)
(defparameter *penv* nil)

(defparameter *process-envs* nil)

(defconstant JNI-VERSION-1-2 #X10002)
(defconstant JNI-VERSION-1-4 #X10004)
(defconstant JNI-OK 0)

(defun load-jni-lib (&optional (libpath *jni-lib-path*))
  (fli:register-module :jni-lib
                     :real-name libpath
                     :connection-style :immediate))

(fli:define-c-typedef pvoid (:ptr :void))
(fli:define-c-typedef const-char-* (:reference-pass :ef-mb-string))
(fli:define-c-typedef const-jchar-* (:reference-pass :ef-wc-string))
(fli:define-foreign-pointer (java-ref (:allow-null t) (:predicate java-ref-p)) pvoid)

(fli:define-c-typedef jvoid :void)
(fli:define-c-typedef jboolean (:boolean (:unsigned :byte)))
(fli:define-c-typedef jbyte :byte)
(fli:define-c-typedef jchar :wchar-t)
(fli:define-c-typedef jshort :short)
(fli:define-c-typedef jint :int)
(fli:define-c-typedef jlong :long-long)
(fli:define-c-typedef jfloat :float)
(fli:define-c-typedef jdouble :double)
(fli:define-c-typedef jsize jint)
(fli:define-c-typedef jobject java-ref)
(fli:define-c-typedef jclass java-ref)
(fli:define-c-typedef jthrowable java-ref)
(fli:define-c-typedef jstring java-ref)
(fli:define-c-typedef jarray java-ref)
(fli:define-c-typedef jboolean-array java-ref)
(fli:define-c-typedef jbyte-array java-ref)
(fli:define-c-typedef jchar-array java-ref)
(fli:define-c-typedef jshort-array java-ref)
(fli:define-c-typedef jint-array java-ref)
(fli:define-c-typedef jlong-array java-ref)
(fli:define-c-typedef jfloat-array java-ref)
(fli:define-c-typedef jdouble-array java-ref)
(fli:define-c-typedef jobject-array java-ref)
(fli:define-c-typedef jfield-id pvoid)
(fli:define-c-typedef jmethod-id pvoid)
(fli:define-c-typedef jweak java-ref)

(fli:define-c-typedef pvm (:ptr (:ptr java-vm)))
(fli:define-c-typedef penv (:ptr (:ptr jni-env)))
(fli:define-foreign-type pfunc (&rest fargs)
  `(:ptr (:function ,@fargs)))

(fli:define-c-union jvalue
  (:z jboolean)
  (:b jbyte)
  (:c jchar)
  (:s jshort)
  (:i jint)
  (:j jlong)
  (:f jfloat)
  (:d jdouble)
  (:l jobject))

(fli:define-c-typedef arg-array (:c-array jvalue))

(eval-when (:compile-toplevel)
  (defun build-struct-entries (name members)
    (mapcar #'(lambda (member)
                (if (= 2 (length member)) ;padding or other non-function entry
                    member
                  (destructuring-bind (func args ret &key lambda-list) member
                    (declare (ignore lambda-list))
                    `(,func (pfunc ,(cons `(:ptr (:ptr ,name))
                                          (mapcar #'second args))
                                   ,ret)))))
            members)))

(eval-when (:compile-toplevel)
  (defun build-access-functions (name global members)
    (mapcar #'(lambda (member)
                (if (= 2 (length member)) ;padding or other non-function entry
                    ()
                  (destructuring-bind (func args ret &key lambda-list) member
                    (let ((thunk (intern (concatenate 'string (symbol-name func) "-thunk")))
                          (genv (gensym))
                        ;(func (intern (symbol-name f)))
                          )
                      `(locally
                         (fli:define-foreign-funcallable
                          ,thunk
                          ,(cons `(this (:ptr (:ptr ,name))) args)
                          :result-type ,ret)
                         (defun ,func ,(if lambda-list
                                           lambda-list
                                         (mapcar #'first args))
                           (let ((,genv ,global))
                             (,thunk
                              (fli:foreign-slot-value (fli:dereference ,genv) ',func)
                              ,genv
                              ,@(mapcar #'first args))))
                         (export ',func))))))
            members)))

(defmacro defvtable (name global &rest members)
  `(locally
     (fli:define-c-struct ,name ,@(build-struct-entries name members))
     ,@(build-access-functions name global members)))

(fli:define-c-struct jni-native-method
  (name (:ptr :char))
  (signature (:ptr :char))
  (fn-ptr pvoid)
  )

(defun current-env ()
  "memoizes attach-current-thread per process"
  (or
   *penv*
   (cdr (assoc mp:*current-process* *process-envs*))
   (multiple-value-bind (ret env) (attach-current-thread)
     (declare (ignore ret))
     (push (cons mp:*current-process* env) *process-envs*)
     env)))

(defvtable jni-env (current-env)
           (reserved-0 pvoid)                                              ;0
           (reserved-1 pvoid)                                              ;1
           (reserved-2 pvoid)                                              ;2
           (reserved-3 pvoid)                                              ;3
  ;some mac nonsense requires this non-portable padding, so much for a binary spec
           #+:MACOSX  (cfm-padding (:foreign-array pvoid (225)))
           (get-version () jint)                                           ;4
           (define-class ((name const-char-*)                              ;5
                          (loader jobject)
                          (buf (:ptr jbyte))
                          (len jsize)) jclass) 
           (jni-find-class ((name const-char-*)) jclass)                       ;6
           (from-reflected-method ((method jobject)) jmethod-id)           ;7
           (from-reflected-field ((field jobject)) jfield-id)              ;8
           (to-reflected-method ((cls jclass)                              ;9
                                 (method-id jmethod-id)
                                 (is-static jboolean)) jobject)
           (get-superclass ((clazz jclass)) jclass)                        ;10
           (is-assignable-from ((sub jclass)                               ;11
                                (sup jclass)) jboolean)
           (to-reflected-field ((cls jclass)                               ;12
                                (field-id jfield-id)
                                (is-static jboolean)) jobject)
           (jni-throw ((obj jthrowable)) jint)                                 ;13
           (throw-new ((clazz jclass)                                      ;14
                       (msg const-char-*)) jint)
           (exception-occurred () jthrowable)                              ;15
           (exception-describe () :void)                                   ;16
           (exception-clear () :void)                                      ;17
           (fatal-error ((msg const-char-*)) :void)                        ;18
           (push-local-frame ((capacity jint)) jint)                       ;19
           (pop-local-frame ((result jobject)) jobject)                    ;20
           (new-global-ref ((lobj jobject)) jobject)                       ;21
           (delete-global-ref ((gref jobject)) :void)                      ;22
           (delete-local-ref ((lref jobject)) :void)                       ;23
           (is-same-object ((obj1 jobject)                                 ;24
                            (obj2 jobject)) jboolean)
           (new-local-ref ((ref jobject)) jobject)                         ;25
           (ensure-local-capacity ((capacity jint)) jint)                  ;26
           (alloc-object ((clazz jclass)) jobject)                         ;27
           (new-object pvoid)                                              ;28
           (new-object-v pvoid)                                            ;29
           (new-object-a ((clazz jclass)                                   ;30
                          (method-id jmethod-id)
                          (args arg-array)) jobject)
           (get-object-class ((obj jobject)) jclass)                       ;31
           (is-instance-of ((obj jobject)                                  ;32
                            (clazz jclass)) jboolean)
           (get-method-id ((clazz jclass)                                  ;33
                           (name const-char-*)
                           (sig const-char-*)) jmethod-id)

           (call-object-method pvoid)                                      ;34
           (call-object-method-v pvoid)                                    ;35
           (call-object-method-a ((obj jobject)                            ;36
                                  (method-id jmethod-id)
                                  (args arg-array)) jobject)
           (call-boolean-method pvoid)                                     ;37
           (call-boolean-method-v pvoid)                                   ;38
           (call-boolean-method-a ((obj jobject)                           ;39
                                   (method-id jmethod-id)                  
                                   (args arg-array)) jboolean)
           (call-byte-method pvoid)                                        ;40
           (call-byte-method-v pvoid)                                      ;41
           (call-byte-method-a ((obj jobject)                              ;42
                                (method-id jmethod-id)
                                (args arg-array)) jbyte)
           (call-char-method pvoid)                                        ;43
           (call-char-method-v pvoid)                                      ;44
           (call-char-method-a ((obj jobject)                              ;45
                                (method-id jmethod-id)
                                (args arg-array)) jchar)
           (call-short-method pvoid)                                       ;46
           (call-short-method-v pvoid)                                     ;47
           (call-short-method-a ((obj jobject)                             ;48
                                 (method-id jmethod-id)
                                 (args arg-array)) jshort)
           (call-int-method pvoid)                                         ;49
           (call-int-method-v pvoid)                                       ;50
           (call-int-method-a ((obj jobject)                               ;51
                               (method-id jmethod-id)
                               (args arg-array)) jint)
           (call-long-method pvoid)                                        ;52
           (call-long-method-v pvoid)                                      ;53
           (call-long-method-a ((obj jobject)                              ;54
                                (method-id jmethod-id)
                                (args arg-array)) jlong)
           (call-float-method pvoid)                                       ;55
           (call-float-method-v pvoid)                                     ;56
           (call-float-method-a ((obj jobject)                             ;57
                                 (method-id jmethod-id)
                                 (args arg-array)) jfloat)
           (call-double-method pvoid)                                      ;58
           (call-double-method-v pvoid)                                    ;59
           (call-double-method-a ((obj jobject)                            ;60
                                  (method-id jmethod-id)
                                  (args arg-array)) jdouble)
           (call-void-method pvoid)                                        ;61
           (call-void-method-v pvoid)                                      ;62
           (call-void-method-a ((obj jobject)                              ;63
                                (method-id jmethod-id)
                                (args arg-array)) jvoid)

           (call-nonvirtual-object-method pvoid)                           ;64
           (call-nonvirtual-object-method-v pvoid)                         ;65
           (call-nonvirtual-object-method-a ((obj jobject)                 ;66
                                             (clazz jclass)
                                             (method-id jmethod-id)
                                             (args arg-array)) jobject)
           (call-nonvirtual-boolean-method pvoid)                          ;67
           (call-nonvirtual-boolean-method-v pvoid)                        ;68
           (call-nonvirtual-boolean-method-a ((obj jobject)                ;69
                                              (clazz jclass)
                                              (method-id jmethod-id)
                                              (args arg-array)) jboolean)
           (call-nonvirtual-byte-method pvoid)                             ;70
           (call-nonvirtual-byte-method-v pvoid)                           ;71
           (call-nonvirtual-byte-method-a ((obj jobject)                   ;72
                                           (clazz jclass)
                                           (method-id jmethod-id)
                                           (args arg-array)) jbyte)
           (call-nonvirtual-char-method pvoid)                             ;73
           (call-nonvirtual-char-method-v pvoid)                           ;74
           (call-nonvirtual-char-method-a ((obj jobject)                   ;75
                                           (clazz jclass)
                                           (method-id jmethod-id)
                                           (args arg-array)) jchar)
           (call-nonvirtual-short-method pvoid)                            ;76
           (call-nonvirtual-short-method-v pvoid)                          ;77
           (call-nonvirtual-short-method-a ((obj jobject)                  ;78
                                            (clazz jclass)
                                            (method-id jmethod-id)
                                            (args arg-array)) jshort)
           (call-nonvirtual-int-method pvoid)                              ;79
           (call-nonvirtual-int-method-v pvoid)                            ;80
           (call-nonvirtual-int-method-a ((obj jobject)                    ;81
                                          (clazz jclass)
                                          (method-id jmethod-id)
                                          (args arg-array)) jint)
           (call-nonvirtual-long-method pvoid)                             ;82
           (call-nonvirtual-long-method-v pvoid)                           ;83
           (call-nonvirtual-long-method-a ((obj jobject)                   ;84
                                           (clazz jclass)
                                           (method-id jmethod-id)
                                           (args arg-array)) jlong)
           (call-nonvirtual-float-method pvoid)                            ;85
           (call-nonvirtual-float-method-v pvoid)                          ;86
           (call-nonvirtual-float-method-a ((obj jobject)                  ;87
                                            (clazz jclass)
                                            (method-id jmethod-id)
                                            (args arg-array)) jfloat)
           (call-nonvirtual-double-method pvoid)                           ;88
           (call-nonvirtual-double-method-v pvoid)                         ;89
           (call-nonvirtual-double-method-a ((obj jobject)                 ;90
                                             (clazz jclass)
                                             (method-id jmethod-id)
                                             (args arg-array)) jdouble)
           (call-nonvirtual-void-method pvoid)                             ;91
           (call-nonvirtual-void-method-v pvoid)                           ;92
           (call-nonvirtual-void-method-a ((obj jobject)                   ;93
                                           (clazz jclass)
                                           (method-id jmethod-id)
                                           (args arg-array)) jvoid)
           (get-field-id ((clazz jclass)                                   ;94
                          (name const-char-*)
                          (sig const-char-*)) jfield-id)

           (get-object-field ((obj jobject)                                ;95
                              (field-id jfield-id)) jobject)
           (get-boolean-field ((obj jobject)                               ;96
                               (field-id jfield-id)) jboolean)  
           (get-byte-field ((obj jobject)                                  ;97
                            (field-id jfield-id)) jbyte)  
           (get-char-field ((obj jobject)                                  ;98
                            (field-id jfield-id)) jchar)  
           (get-short-field ((obj jobject)                                 ;99
                             (field-id jfield-id)) jshort)  
           (get-int-field ((obj jobject)                                   ;100
                           (field-id jfield-id)) jint)  
           (get-long-field ((obj jobject)                                  ;101
                            (field-id jfield-id)) jlong)  
           (get-float-field ((obj jobject)                                 ;102
                             (field-id jfield-id)) jfloat)  
           (get-double-field ((obj jobject)                                ;103
                              (field-id jfield-id)) jdouble)  

           (set-object-field ((obj jobject)                                ;104
                              (field-id jfield-id)
                              (val jobject)) jvoid)
           (set-boolean-field ((obj jobject)                               ;105
                               (field-id jfield-id)
                               (val jboolean)) jvoid)
           (set-byte-field ((obj jobject)                                  ;106
                            (field-id jfield-id)
                            (val jbyte)) jvoid)
           (set-char-field ((obj jobject)                                  ;107
                            (field-id jfield-id)
                            (val jchar)) jvoid)
           (set-short-field ((obj jobject)                                 ;108
                             (field-id jfield-id)
                             (val jshort)) jvoid)
           (set-int-field ((obj jobject)                                   ;109
                           (field-id jfield-id)
                           (val jint)) jvoid)
           (set-long-field ((obj jobject)                                  ;110
                            (field-id jfield-id)
                            (val jlong)) jvoid)
           (set-float-field ((obj jobject)                                 ;111
                             (field-id jfield-id)
                             (val jfloat)) jvoid)
           (set-double-field ((obj jobject)                                ;112
                              (field-id jfield-id)
                              (val jdouble)) jvoid)

           (get-static-method-id ((clazz jclass)                           ;113
                                  (name const-char-*)
                                  (sig const-char-*)) jmethod-id)

           (call-static-object-method pvoid)                               ;114
           (call-static-object-method-v pvoid)                             ;115
           (call-static-object-method-a ((clazz jclass)                    ;116
                                         (method-id jmethod-id)
                                         (args arg-array)) jobject)
           (call-static-boolean-method pvoid)                              ;117
           (call-static-boolean-method-v pvoid)                            ;118
           (call-static-boolean-method-a ((clazz jclass)                   ;119
                                          (method-id jmethod-id)
                                          (args arg-array)) jboolean)
           (call-static-byte-method pvoid)                                 ;120
           (call-static-byte-method-v pvoid)                               ;121
           (call-static-byte-method-a ((clazz jclass)                      ;122
                                       (method-id jmethod-id)
                                       (args arg-array)) jbyte)
           (call-static-char-method pvoid)                                 ;123
           (call-static-char-method-v pvoid)                               ;124
           (call-static-char-method-a ((clazz jclass)                      ;125
                                       (method-id jmethod-id)
                                       (args arg-array)) jchar)
           (call-static-short-method pvoid)                                ;126
           (call-static-short-method-v pvoid)                              ;127
           (call-static-short-method-a ((clazz jclass)                     ;128
                                        (method-id jmethod-id)
                                        (args arg-array)) jshort)
           (call-static-int-method pvoid)                                  ;129
           (call-static-int-method-v pvoid)                                ;130
           (call-static-int-method-a ((clazz jclass)                       ;131
                                      (method-id jmethod-id)
                                      (args arg-array)) jint)
           (call-static-long-method pvoid)                                 ;132
           (call-static-long-method-v pvoid)                               ;133
           (call-static-long-method-a ((clazz jclass)                      ;134
                                       (method-id jmethod-id)
                                       (args arg-array)) jlong)
           (call-static-float-method pvoid)                                ;135
           (call-static-float-method-v pvoid)                              ;136
           (call-static-float-method-a ((clazz jclass)                     ;137
                                        (method-id jmethod-id)
                                        (args arg-array)) jfloat)
           (call-static-double-method pvoid)                               ;138
           (call-static-double-method-v pvoid)                             ;139
           (call-static-double-method-a ((clazz jclass)                    ;140
                                         (method-id jmethod-id)
                                         (args arg-array)) jdouble)
           (call-static-void-method pvoid)                                 ;141
           (call-static-void-method-v pvoid)                               ;142
           (call-static-void-method-a ((clazz jclass)                      ;143
                                       (method-id jmethod-id)
                                       (args arg-array)) jvoid)

           (get-static-field-id ((clazz jclass)                            ;144
                                 (name const-char-*)
                                 (sig const-char-*)) jfield-id)

           (get-static-object-field ((clazz jclass)                        ;145
                                     (field-id jfield-id)) jobject)
           (get-static-boolean-field ((clazz jclass)                       ;146
                                      (field-id jfield-id)) jboolean)
           (get-static-byte-field ((clazz jclass)                          ;147
                                   (field-id jfield-id)) jbyte)
           (get-static-char-field ((clazz jclass)                          ;148
                                   (field-id jfield-id)) jchar)
           (get-static-short-field ((clazz jclass)                         ;149
                                    (field-id jfield-id)) jshort)
           (get-static-int-field ((clazz jclass)                           ;150
                                  (field-id jfield-id)) jint)
           (get-static-long-field ((clazz jclass)                          ;151
                                   (field-id jfield-id)) jlong)
           (get-static-float-field ((clazz jclass)                         ;152
                                    (field-id jfield-id)) jfloat)
           (get-static-double-field ((clazz jclass)                        ;153
                                     (field-id jfield-id)) jdouble)

           (set-static-object-field ((clazz jclass)                        ;154
                                     (field-id jfield-id)
                                     (val jobject)) jvoid)
           (set-static-boolean-field ((clazz jclass)                       ;155
                                      (field-id jfield-id)
                                      (val jboolean)) jvoid)
           (set-static-byte-field ((clazz jclass)                          ;156
                                   (field-id jfield-id)
                                   (val jbyte)) jvoid)
           (set-static-char-field ((clazz jclass)                          ;157
                                   (field-id jfield-id)
                                   (val jchar)) jvoid)
           (set-static-short-field ((clazz jclass)                         ;158
                                    (field-id jfield-id)
                                    (val jshort)) jvoid)
           (set-static-int-field ((clazz jclass)                           ;159
                                  (field-id jfield-id)
                                  (val jint)) jvoid)
           (set-static-long-field ((clazz jclass)                          ;160
                                   (field-id jfield-id)
                                   (val jlong)) jvoid)
           (set-static-float-field ((clazz jclass)                         ;161
                                    (field-id jfield-id)
                                    (val jfloat)) jvoid)
           (set-static-double-field ((clazz jclass)                        ;162
                                     (field-id jfield-id)
                                     (val jdouble)) jvoid)

           (new-string ((uchars (:reference-pass :ef-wc-string))               ;163
                        (len jsize)) jstring)
           (get-string-length ((str jstring)) jsize)                       ;164
           (get-string-chars ((str jstring)                                ;165
                              (is-copy (:reference-return jboolean)))
                             ;(:c-array jchar 1000)
                             (:ptr :wchar-t)
                             ;(:ef-wc-string :external-format :unicode)
                             :lambda-list (str &optional is-copy))
           (release-string-chars ((str jstring)                            ;166
                                  (chars (:ptr jchar))) jvoid)

           (new-string-utf ((chars const-char-*)) jstring)                 ;167
           (get-string-utf-length ((str jstring)) jsize)                   ;168
           (get-string-utf-chars ((str jstring)                            ;169
                                  (is-copy (:reference-return jboolean)))
                                 ;(:c-array :char 1000)
                                 (:ptr :char)
                                 :lambda-list (str &optional is-copy))
           (release-string-utf-chars ((str jstring)                        ;170
                                      (chars (:ptr :char))) jvoid)

           (get-array-length ((array jarray)) jsize)                       ;171
           
           (new-object-array ((len jsize)                                  ;172
                              (element-type jclass)
                              (initial-element jobject)) jarray)
           (get-object-array-element ((array jobject-array)                ;173
                                      (index jsize)) jobject)
           (set-object-array-element ((array jobject-array)                ;174
                                      (index jsize)
                                      (val jobject)) jvoid)

           (new-boolean-array ((len jsize)) jboolean-array)                ;175
           (new-byte-array ((len jsize)) jbyte-array)                      ;176
           (new-char-array ((len jsize)) jchar-array)                      ;177
           (new-short-array ((len jsize)) jshort-array)                    ;178
           (new-int-array ((len jsize)) jint-array)                        ;179
           (new-long-array ((len jsize)) jlong-array)                      ;180
           (new-float-array ((len jsize)) jfloat-array)                    ;181
           (new-double-array ((len jsize)) jdouble-array)                  ;182

           (get-boolean-array-elements ((array jboolean-array)             ;183
                                        (is-copy (:reference-return jboolean)))
                                       (:ptr jboolean)
                                       :lambda-list (array &optional is-copy))
           (get-byte-array-elements ((array jbyte-array)                   ;184
                                     (is-copy (:reference-return jboolean)))
                                    (:ptr jbyte)
                                    :lambda-list (array &optional is-copy))
           (get-char-array-elements ((array jchar-array)                   ;185
                                     (is-copy (:reference-return jboolean)))
                                    (:ptr jchar)
                                    :lambda-list (array &optional is-copy))
           (get-short-array-elements ((array jshort-array)                 ;186
                                      (is-copy (:reference-return jboolean)))
                                     (:ptr jshort)
                                     :lambda-list (array &optional is-copy))
           (get-int-array-elements ((array jint-array)                     ;187
                                    (is-copy (:reference-return jboolean)))
                                   (:ptr jint)
                                   :lambda-list (array &optional is-copy))
           (get-long-array-elements ((array jlong-array)                   ;188
                                     (is-copy (:reference-return jboolean)))
                                    (:ptr jlong)
                                    :lambda-list (array &optional is-copy))
           (get-float-array-elements ((array jfloat-array)                 ;189
                                      (is-copy (:reference-return jboolean)))
                                     (:ptr jfloat)
                                     :lambda-list (array &optional is-copy))
           (get-double-array-elements ((array jdouble-array)               ;190
                                       (is-copy (:reference-return jboolean)))
                                      (:ptr jdouble)
                                      :lambda-list (array &optional is-copy))

           (release-boolean-array-elements ((array jboolean-array)         ;191
                                            (elems (:ptr jboolean))
                                            (mode jint)) jvoid
                                           :lambda-list (array elems &optional (mode 0)))
           (release-byte-array-elements ((array jbyte-array)               ;192
                                         (elems (:ptr jbyte))
                                         (mode jint)) jvoid
                                        :lambda-list (array elems &optional (mode 0)))
           (release-char-array-elements ((array jchar-array)               ;193
                                         (elems (:ptr jchar))
                                         (mode jint)) jvoid
                                        :lambda-list (array elems &optional (mode 0)))
           (release-short-array-elements ((array jshort-array)             ;194
                                          (elems (:ptr jshort))
                                          (mode jint)) jvoid
                                         :lambda-list (array elems &optional (mode 0)))
           (release-int-array-elements ((array jint-array)                 ;195
                                        (elems (:ptr jint))
                                        (mode jint)) jvoid
                                       :lambda-list (array elems &optional (mode 0)))
           (release-long-array-elements ((array jlong-array)               ;196
                                         (elems (:ptr jlong))
                                         (mode jint)) jvoid
                                        :lambda-list (array elems &optional (mode 0)))
           (release-float-array-elements ((array jfloat-array)             ;197
                                          (elems (:ptr jfloat))
                                          (mode jint)) jvoid
                                         :lambda-list (array elems &optional (mode 0)))
           (release-double-array-elements ((array jdouble-array)           ;198
                                           (elems (:ptr jdouble))
                                           (mode jint)) jvoid
                                          :lambda-list (array elems &optional (mode 0)))

           (get-boolean-array-region ((array jboolean-array)               ;199
                                      (start jsize)
                                      (len jsize)
                                      (buf (:ptr jboolean))) jvoid)
           (get-byte-array-region ((array jbyte-array)                     ;200
                                   (start jsize)
                                   (len jsize)
                                   (buf (:ptr jbyte))) jvoid)
           (get-char-array-region ((array jchar-array)                     ;201
                                   (start jsize)
                                   (len jsize)
                                   (buf (:ptr jchar))) jvoid)
           (get-short-array-region ((array jshort-array)                   ;202
                                    (start jsize)
                                    (len jsize)
                                    (buf (:ptr jshort))) jvoid)
           (get-int-array-region ((array jint-array)                       ;203
                                  (start jsize)
                                  (len jsize)
                                  (buf (:ptr jint))) jvoid)
           (get-long-array-region ((array jlong-array)                     ;204
                                   (start jsize)
                                   (len jsize)
                                   (buf (:ptr jlong))) jvoid)
           (get-float-array-region ((array jfloat-array)                   ;205
                                    (start jsize)
                                    (len jsize)
                                    (buf (:ptr jfloat))) jvoid)
           (get-double-array-region ((array jdouble-array)                 ;206
                                     (start jsize)
                                     (len jsize)
                                     (buf (:ptr jdouble))) jvoid)

           (set-boolean-array-region ((array jboolean-array)               ;207
                                      (start jsize)
                                      (len jsize)
                                      (buf (:ptr jboolean))) jvoid)
           (set-byte-array-region ((array jbyte-array)                     ;208
                                   (start jsize)
                                   (len jsize)
                                   (buf (:ptr jbyte))) jvoid)
           (set-char-array-region ((array jchar-array)                     ;209
                                   (start jsize)
                                   (len jsize)
                                   (buf (:ptr jchar))) jvoid)
           (set-short-array-region ((array jshort-array)                   ;210
                                    (start jsize)
                                    (len jsize)
                                    (buf (:ptr jshort))) jvoid)
           (set-int-array-region ((array jint-array)                       ;211
                                  (start jsize)
                                  (len jsize)
                                  (buf (:ptr jint))) jvoid)
           (set-long-array-region ((array jlong-array)                     ;212
                                   (start jsize)
                                   (len jsize)
                                   (buf (:ptr jlong))) jvoid)
           (set-float-array-region ((array jfloat-array)                   ;213
                                    (start jsize)
                                    (len jsize)
                                    (buf (:ptr jfloat))) jvoid)
           (set-double-array-region ((array jdouble-array)                 ;214
                                     (start jsize)
                                     (len jsize)
                                     (buf (:ptr jdouble))) jvoid)

           (register-natives ((clazz jclass)                               ;215
                              (methods (:ptr jni-native-method))
                              (n-methods jsize)) jint)
           (unregister-natives ((clazz jclass)) jint)                      ;216
           (monitor-enter ((obj jobject)) jint)                            ;217
           (monitor-exit ((obj jobject)) jint)                             ;218

           (get-java-vm ((vm (:reference-return pvm))) jint                ;219
                        :lambda-list (&optional (vm t)))

           (get-string-region ((str jstring)                               ;220
                               (start jsize)
                               (len jsize)
                               (buf (:ptr jchar))) jvoid)
           (get-string-utf-region ((str jstring)                           ;221
                               (start jsize)
                               (len jsize)
                               (buf (:ptr :char))) jvoid)

           (get-primitive-array-critical ((array jarray)                   ;222
                                          (is-copy (:reference-return jboolean))) pvoid
                                         :lambda-list (array &optional is-copy))
           (release-primitive-array-critical ((array jarray)               ;223
                                            (carray pvoid)
                                            (mode jint)) jvoid
                                           :lambda-list (array carray &optional (mode 0)))
           
           (get-string-critical ((str jstring)                             ;224
                                 (is-copy (:reference-return jboolean)))
                                (:ptr jchar)
                                :lambda-list (str &optional is-copy))
           (release-string-critical ((str jstring)                         ;225
                                     (cstring (:ptr jchar))) jvoid)
           (new-weak-global-ref ((obj jobject)) jweak)                     ;226
           (delete-weak-global-ref ((ref jweak)) jvoid)                    ;227
           (exception-check () jboolean)                                   ;228
           )

(defun get-pvm ()
  (or *pvm*
      (error "JVM not loaded")))

(defvtable java-vm (get-pvm)
  (reserved-0 pvoid)
  (reserved-1 pvoid)
  (reserved-2 pvoid)
#+:MACOSX  (cfm-padding (:foreign-array pvoid (4)))
  (destroy-java-vm () jint)
  (attach-current-thread ((penv (:reference-return penv)) (args pvoid)) jint
                         :lambda-list (&optional args (penv t)))
  (detach-current-thread () jint)
  (get-env ((penv (:reference-return penv)) (interface-id jint)) jint
           :lambda-list (interface-id &optional (penv t))))
  
(fli:define-c-struct java-vm-option
  (option-string (:ptr :char))
  (extra-info pvoid))

(fli:define-c-struct jdk-1-1-init-args
  (version jint)
  (properties (:ptr (:ptr char)))
  (check-source jint)
  (native-stack-size jint)
  (java-stack-size jint)
  (min-heap-size jint)
  (max-heap-size jint)
  (verify-mode jint)
  (class-path (:ptr :char))
  (vprintf pvoid)
  (exit pvoid)
  (abort pvoid)
  (enable-class-gc jint)
  (enable-verbose-gc jint)
  (disable-async-gc jint)
  (reserved-0 jint)
  (reserved-1 jint)
  (reserved-2 jint))
  
(fli:define-foreign-function (jni-get-default-java-vm-init-args "JNI_GetDefaultJavaVMInitArgs")
    ((init-args (:ptr jdk-1-1-init-args)))
  :result-type jint)

(fli:define-c-struct java-vm-init-args
  (version jint)
  (n-options jint)
  (options (:ptr java-vm-option))
  (ignore-unrecognized jboolean))

(fli:define-foreign-function (jni-create-java-vm "JNI_CreateJavaVM" :source)
    ((pvm (:reference-return pvm))
     (penv (:reference-return penv))
     (vm-args (:ptr java-vm-init-args)))
  :result-type jint
  :lambda-list (vm-args &optional (pvm t) (penv t))
;  :module :jni-lib ;refused on Mac OSX, even though register-module is supported
  )

(fli:define-foreign-function (jni-get-created-java-vms "JNI_GetCreatedJavaVMs" :source)
    ((vm-buf (:c-array pvm))
     (buf-len jsize)
     (n-vms (:reference-return jsize)))
  :result-type jint)

(defun cleanup-jni-gref (gref)
  "set as a special free action to free java classes when no longer used by Lisp"
  (when (java-ref-p gref)
    (delete-global-ref gref)))

(defun create-jvm (&rest option-strings)
  "Creates the JVM, this can only be done once.
The option strings can be used to control the JVM, esp. the classpath:
\"-Djava.class.path=/Users/rich/Lisp/jfli.jar\""
  (when *pvm*
    (error "JVM already created, can only be started once"))
  (load-jni-lib)
  (let ((nopts (length option-strings))
         (option-array nil))
    (fli:with-dynamic-foreign-objects ((ia java-vm-init-args))
      (when option-strings
        (setf option-array (fli:allocate-dynamic-foreign-object :type 'java-vm-option :nelems nopts))
        (dotimes (n nopts)
          (setf (fli:foreign-slot-value (fli:dereference option-array
                                                         :index n
                                                         :copy-foreign-object nil) 'option-string)
                (fli:convert-to-dynamic-foreign-string (nth n option-strings)))))
      (fli:with-foreign-slots (VERSION N-OPTIONS OPTIONS IGNORE-UNRECOGNIZED) ia
        (setf version JNI-VERSION-1-4
              n-options nopts
              OPTIONS option-array
              IGNORE-UNRECOGNIZED nil)
        (multiple-value-bind (ret vm env)
            (jni-create-java-vm ia)
          (setf *pvm* vm)
          (add-special-free-action #'cleanup-jni-gref)
          (values ret vm env))))))

;this is the FLI side of proxy support

(defvar *invocation-handler* nil
  "this will be set by jfli:enable-java-proxies to a function of 3 args")

;this will be set as the implementation of a native java function
(fli:define-foreign-callable ("LispInvocationHandler_invoke" :result-type jobject)
    ((env penv) (obj jobject) (proxy jobject) (method jobject) (args jobject))
  (do-invoke env obj proxy method args))

(defun do-invoke (env obj proxy method args)
  ;(declare (ignore env))
  (when *invocation-handler*
    (let ((*penv* env))
      (prog1
          (funcall *invocation-handler* proxy method args)
        ;(jfli::invocation-handler proxy method args)
        (delete-local-ref obj)))))

(defun register-invocation-handler (invocation-handler)
  "sets up the Lisp handler and binds the native function - jfli.jar must be in the classpath"
  (setf *invocation-handler* invocation-handler)
  (fli:with-dynamic-foreign-objects ((method jni-native-method))
    (let ((lih (try-null (jni-find-class "com/richhickey/jfli/LispInvocationHandler"))))
      (fli:with-foreign-slots (name signature fn-ptr) method
        (setf name (fli:convert-to-dynamic-foreign-string "invoke")
              signature (fli:convert-to-dynamic-foreign-string "(Ljava/lang/Object;Ljava/lang/reflect/Method;[Ljava/lang/Object;)Ljava/lang/Object;")
              fn-ptr (fli:make-pointer :symbol-name "LispInvocationHandler_invoke")))
      (register-natives lih method 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the code below provides for the generation of wrapper functions that use JNI to access
;methods and fields. This low-level interface is unsafe, in that JNI will not 
;check arg types etc on calls, and therefore should only be used to build safer high-level interfaces
;i.e. use jfli!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;found on c.l.l
(eval-when (:compile-toplevel :load-toplevel)
(defun replace-substrings (string substring replacement)
  (declare (optimize (speed 3))
           (type simple-string string substring replacement))
  (assert (> (length substring) 0) (substring)
    "Substring ~A must be of length ~D > 0"
    substring (length substring))
  (with-output-to-string (stream)
    (loop with substring-length = (length substring)
          for index = 0 then (+ match-index substring-length)
          for match-index = (search substring string :start2 index)
          do
          (write-string string stream :start index :end match-index)
          (when match-index
            (write-string replacement stream))
          while match-index)))


(defun local-ref-to-global-ref (lref)
  (when lref
    (let ((gref (new-global-ref lref)))
      (flag-special-free-action gref)
      (delete-local-ref lref)
      gref)))

(defun local-ref-to-string (lref)
  (prog1
      (convert-from-java-string lref)
    (delete-local-ref lref)))

(defun convert-to-java-string (s)
  (when s
    (try-null (new-string-utf (string s)))))

(defun convert-from-java-string (s)
  (when s
    (let ((chars (try-null (get-string-utf-chars s))))
      (prog1
          (fli:convert-from-foreign-string chars :external-format :utf-8)
        (release-string-utf-chars s chars)))))

(defun jaref (array index)
  (try (get-object-array-element array index)))

(defun (setf jaref) (val array index)
  (try (set-object-array-element array index val)))

(defun convert-string-arg (s)
  "if s is stringp, make into java string, else presume it is a java string and return it"
  ;presumably faster than checking if s is a foreign pointer?
  (if (or (stringp s) (symbolp s))
      (convert-to-java-string s)
    s))

(defun process-arg (val type)
  (if (string-equal "java.lang.String" type)
                 `(convert-string-arg ,val)
                 val))

(defmacro set-arg (args i val type)
  `(setf (fli:foreign-slot-value (fli:dereference (fli:foreign-array-pointer ,args ,i)
                                                     :copy-foreign-object nil)
                                    ',(slot-from-typename type))
            ,(process-arg val type)))

(defmacro with-arg-array (arg-array-name args &body body)
  (let ((i -1))
  `(fli:with-dynamic-foreign-objects ()
     (let ((,arg-array-name
            (fli:allocate-dynamic-foreign-object :type
                                                 '(:c-array jvalue ,(length args)))))
       ,@(mapcar #'(lambda (arg)
                     (list 'set-arg arg-array-name (incf i) (first arg) (second arg))) 
                 args)

       ,@body))))

(defun build-descriptor (params return-type)
  (string-append
   "("
   (apply #'string-append (mapcar #'(lambda (p)
                                      (type-descriptor-from-typename (second p)))
                                  params))
   ")"
   (type-descriptor-from-typename return-type)))

(defun get-class-and-method-id (class-name method-name descriptor is-static)
  (let ((class (local-ref-to-global-ref
                (try-null (jni-find-class class-name)))))
    (values class
            (if is-static
                (try-null (get-static-method-id class method-name descriptor))
              (try-null (get-method-id class method-name descriptor))))))


(defun get-class-and-field-id (class-name field-name descriptor is-static)
  (let ((class (local-ref-to-global-ref
                (try-null (jni-find-class class-name)))))
    (values class
            (if is-static
                (try-null (get-static-field-id class field-name descriptor))
              (try-null (get-field-id class field-name descriptor))))))

(defun is-name-of-primitive (s)
  (member s '("boolean" "byte" "char" "short" "int" "long" "float" "double" "void")
          :test #'string-equal))

(defun package-qualified-name (classname packagename)
  (cond
   ((is-name-of-primitive (subseq classname 0 (position #\< classname))) classname)
   ((find #\. classname) classname)     ;already qualified, presumably by another package
   (t (string-append packagename "." classname)))) 

(defun split-package-and-class (name)
    (let ((p (position #\. name :from-end t)))
      (unless p (error "must supply package-qualified classname"))
      (values (subseq name 0 p)
              (subseq name (1+ p)))))

(defun slot-from-typename (tn)
  (let ((prim (assoc tn
                     '(("boolean" . :z)
                       ("byte" . :b)
                       ("char" . :c)
                       ("short" . :s)
                       ("int" . :i)
                       ("long" . :j)
                       ("float" . :f)
                       ("double" . :d))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      :l)))

(defun name-component-from-typename (tn)
  (if (is-name-of-primitive tn)
      tn
    "object"))

(defun type-descriptor-from-typename (tn)
  (let ((prim (assoc tn
                     '(("boolean" . "Z")
                       ("byte" . "B")
                       ("char" . "C")
                       ("short" . "S")
                       ("int" . "I")
                       ("long" . "J")
                       ("float" . "F")
                       ("double" . "D")
                       ("void" . "V"))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      (let ((array-depth (count #\< tn))
            (tn-with-slashes (replace-substrings tn "." "/")))
        (if (= 0 array-depth)
            (string-append "L" tn-with-slashes ";")
          (with-output-to-string (s)
            (dotimes (x array-depth)
              (write-string "[" s))
            (write-string (type-descriptor-from-typename
                           (subseq tn-with-slashes 0 (position #\< tn-with-slashes))) s)))))))

;not an exact reciprocal of type-descriptor-from-typename since reflection uses . not / as separator
(defun typename-from-reflection-type-descriptor (tn)
  (let ((prim (assoc tn
                     '(("Z" . "boolean")
                       ("B" . "byte")
                       ("C" . "char")
                       ("S" . "short")
                       ("I" . "int")
                       ("J" . "long")
                       ("F" . "float")
                       ("D" . "double")
                       ("V" . "void"))
                     :test #'string-equal)))
    (if prim
        (rest prim)
      (let ((array-depth (count #\[ tn)))
        (if (= 0 array-depth)
            (subseq tn 1 (1- (length tn))) ;strip leading L and trailing ;
          (with-output-to-string (s)
            (write-string (typename-from-reflection-type-descriptor (subseq tn array-depth)) s)
            (dotimes (x array-depth)
              (write-string "<>" s))))))))

(defun method-name-from-typename (tn static)
    (find-symbol (string-upcase (string-append "call-"
                                               (if static "static-" "")
                                             (name-component-from-typename tn)
                                             "-method-a")) :jni))

(defun field-get-name-from-typename (tn static)
    (find-symbol (string-upcase (string-append "get-"
                                               (if static "static-" "")
                                             (name-component-from-typename tn)
                                             "-field")) :jni))

(defun field-set-name-from-typename (tn static)
    (find-symbol (string-upcase (string-append "set-"
                                               (if static "static-" "")
                                             (name-component-from-typename tn)
                                             "-field")) :jni))
(defun process-return (return-type f &key raw-return)
  (cond
   ((or raw-return (is-name-of-primitive return-type)) f)
   ((string-equal "java.lang.String" return-type) `(local-ref-to-string ,f))
   (t `(local-ref-to-global-ref ,f))))

;JNI wrapper generators - will create functions in current package
;this needs more docs
(defmacro define-java-function (fname class-name return-type method-name params &key static raw-return)
  (let ((this (gensym))
        (class (gensym))
        (id (gensym))
        (args (gensym)))
    `(let (,class ,id)
       (defun ,fname ,(if static (mapcar #'first params)
                        (cons this (mapcar #'first params)))
         (when (null ,class)
           (multiple-value-setq (,class ,id)
               (get-class-and-method-id ,(replace-substrings class-name "." "/")
                                        ,method-name ,(build-descriptor params return-type) ,static)))
         (with-arg-array ,args ,(mapcar #'(lambda (param)
                                           (list (first param) (second param)))
                                       params)
           ,(process-return return-type
                            `(try (,(method-name-from-typename return-type static)
                                   ,(if static class this) ,id ,args))
                            :raw-return raw-return))))))

(defmacro define-java-field (getname class-name field-type field-name &key static)
  (let ((this (gensym))
        (class (gensym))
        (id (gensym))
        (val (gensym)))
    `(let (,class ,id)
       (flet ((load-ids ()
                (when (null ,class)
                  (multiple-value-setq (,class ,id)
                      (get-class-and-field-id ,(replace-substrings class-name "." "/")
                                              ,field-name ,(type-descriptor-from-typename field-type)
                                              ,static)))))
         (defun ,getname ,(if static () (list this))
           (load-ids)
           ,(process-return field-type
                            `(try (,(field-get-name-from-typename field-type static)
                                   ,(if static class this) ,id))))
         (defun (setf ,getname) ,(if static (list val) (list this val))
           (load-ids)
           (try (,(field-set-name-from-typename field-type static)
                 ,(if static class this) ,id ,(process-arg val field-type)))
           ,val)))))

(defmacro define-java-constructor (fname class-name params)
  (let ((class (gensym))
        (id (gensym))
        (args (gensym)))
    `(let (,class ,id)
       (defun ,fname ,(mapcar #'first params)
         (when (null ,class)
           (multiple-value-setq (,class ,id)
               (get-class-and-method-id ,(replace-substrings class-name "." "/")
                                        "<init>" ,(build-descriptor params "void") nil)))
         (with-arg-array ,args ,(mapcar #'(lambda (param)
                                           (list (first param) (second param)))
                                       params)
           (local-ref-to-global-ref (try-null (new-object-a ,class ,id ,args))))))))

(defun make-func-name (class method params append-param-types)
  ;probably a format one-liner that can do this
    (let ((base (string-append class "." method)))
      (if append-param-types
          (string-append base
                         (let ((param-types (mapcar #'second params)))
                           (if param-types
                               (string-append "<"
                                              (reduce #'(lambda (x y)
                                                          (string-append x "-" y)) param-types)
                                              ">")
                             "<>")))
        base)))

;these just do some name twiddling before calling define-java-xxx above
(defmacro def-jni-function (package-and-class method params return-typename
                                               &key static overloaded raw-return)
  (multiple-value-bind (package class) (split-package-and-class package-and-class)
    (let* ((fname (make-func-name class method params overloaded))
           (fsym (read-from-string fname)))
      `(locally ,(list 'define-java-function
                     fsym
                     package-and-class
                     (package-qualified-name return-typename package)
                     method
                     (mapcar #'(lambda (p)
                                 (list (first p) (package-qualified-name (second p) package)))
                             params)
                     :static static :raw-return raw-return)))))

(defmacro def-jni-functions (package-and-class &rest decls)
  `(locally ,@(mapcar #'(lambda (decl)
                          (list* 'def-jni-function package-and-class decl))
                      decls)))

(defmacro def-jni-constructor (package-and-class params &key overloaded)
  (multiple-value-bind (package class) (split-package-and-class package-and-class)
    (let* ((fname (make-func-name class "new" params overloaded))
           (fsym (read-from-string fname)))
      `(locally ,(list 'define-java-constructor
                     fsym 
                     package-and-class 
                     (mapcar #'(lambda (p)
                                 (list (first p) (package-qualified-name (second p) package)))
                             params))))))

(defmacro def-jni-field (package-and-class field typename &key static)
  (multiple-value-bind (package class) (split-package-and-class package-and-class)
    (let ((getsym (read-from-string (string-append class "." field
                                                   (if static "-accessor" ""))))
          (macsym (read-from-string (string-append class "." field))))
      `(locally 
         ,(list 'define-java-field getsym package-and-class
                (package-qualified-name typename package) field :static static)
         ,(when static
            `(define-symbol-macro ,macsym (,getsym)))))))

;we're going to use a little Java to do exception handling below
(def-jni-function "java.lang.Object"
                   "toString" () "String")

(def-jni-function "java.lang.reflect.InvocationTargetException"
                  "getTargetException" () "java.lang.Throwable")

(def-jni-functions "java.lang.Throwable"
                   ("getMessage" () "String")
                   ("getStackTrace" () "StackTraceElement<>"))

(defmacro do-jarray ((x array) &body body)
  (let ((gcount (gensym))
        (gi (gensym))
        (garray (gensym)))
    `(let* ((,garray ,array)
            (,gcount (get-array-length ,garray)))
       (dotimes (,gi ,gcount)
         (let ((,x (jaref ,garray ,gi)))
           ,@body)))))

#|
It is critical that if you call a JNI function that might throw an exception that you clear it,
otherwise the next Java call you make will cause a crash
|#
(defun handle-exception ()
  (let ((e (exception-occurred)))
    (when (not (fli:null-pointer-p e)) ;allow for safe calling in non-exceptional state
      (exception-clear)
      ;if the exception occurs in the reflection target, we really want that
      (when (is-instance-of e (jni-find-class "java/lang/reflect/InvocationTargetException"))
        (setf e (invocationtargetexception.gettargetexception e)))
      (error "~A" (with-output-to-string (s)
                    (format s "~A~%" (object.tostring e))
                    (do-jarray (x (throwable.getstacktrace e))
                      (format s "~A~%" (object.tostring x))))))))

(defun try (result)
  (if (exception-check)
      (handle-exception)
    result))

;JNI will sometimes indicate theere is an exception via a return value
;so take advantage of that when possible vs. the call back to exception-check
(defun try-null (result)
  (if (fli:null-pointer-p result)
      (handle-exception)
    result))

(defun try-neg (result)
  (if (minusp result)
      (handle-exception)
    result))


)

