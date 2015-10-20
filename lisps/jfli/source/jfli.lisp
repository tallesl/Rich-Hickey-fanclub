;    Copyright (c) Rich Hickey. All rights reserved.
;    The use and distribution terms for this software are covered by the
;    Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;    which can be found in the file CPL.TXT at the root of this distribution.
;    By using this software in any fashion, you are agreeing to be bound by
;    the terms of this license.
;    You must not remove this notice, or any other, from this software.

#|

jfli is a library that provides access to Java from Lisp
It depends on the jni package (included)
Callbacks from Java to Lisp also require jfli.jar (included)

|#

(defpackage :jfli
  (:use :common-lisp :lispworks :jni)
  (:export

   ;jvm creation
   :*jni-lib-path*  ;exposed from jni
   :create-jvm      ;exposed from jni, you must call this prior to calling any other jfli function
   :enable-java-proxies

   ;wrapper generation
   :def-java-class
   :get-jar-classnames
   :dump-wrapper-defs-to-file

   ;object creation etc
   :find-java-class
   :new
   :make-new
   :make-typed-ref
   :jeq

   ;array support
   :make-new-array
   :jlength
   :jref
   :jref-boolean
   :jref-byte
   :jref-char
   :jref-double
   :jref-float
   :jref-int
   :jref-short
   :jref-long

   ;proxy support
   :new-proxy
   :unregister-proxy

   ;conversions
   :box-boolean
   :box-byte
   :box-char
   :box-double
   :box-float
   :box-integer
   :box-long
   :box-short
   :box-string
   :unbox-boolean
   :unbox-byte
   :unbox-char
   :unbox-double
   :unbox-float
   :unbox-integer
   :unbox-long
   :unbox-short
   :unbox-string

;   :ensure-package
;   :member-symbol
;   :class-symbol
;   :constructor-symbol
   ))

(in-package :jfli)

#|
bootstrap the implementation of reflection wrappers with 
a few (primitive, less safe and maybe faster) jni wrappers
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-jni-functions "java.lang.Object"
                     ("getClass" () "Class")
                     ("hashCode" () "int")
                     ("toString" () "String")
                     ("equals" ((obj "Object")) "boolean"))

  (def-jni-functions "java.lang.Class"
                   ;should be :overloaded t, but we only use this version
                     ("forName" ((className "String")) "Class"  :static t)
                     ("getConstructors" () "java.lang.reflect.Constructor<>")
                     ("getFields" () "java.lang.reflect.Field<>")
                     ("getMethods" () "java.lang.reflect.Method<>")

                     ("getConstructor" ((parameter-types "Class<>")) "java.lang.reflect.Constructor")
                     ("getField" ((name "String"))
                                 "java.lang.reflect.Field")
                     ("getMethod" ((name "String") (parameter-types "Class<>"))
                                  "java.lang.reflect.Method")

                     ("getSuperclass" () "Class")
                     ("getInterfaces" () "Class<>")

                     ("getName" () "String")
                     ("isArray" () "boolean")
                     ("isPrimitive" () "boolean"))

  (def-jni-functions "java.lang.reflect.Field"
                     ("getName" () "java.lang.String")
                     ("getType" () "java.lang.Class")
                     ("getModifiers" () "int")

                     ("get" ((obj "java.lang.Object")) "java.lang.Object" :raw-return t)
                     ("getBoolean" ((obj "java.lang.Object")) "boolean")
                     ("getByte" ((obj "java.lang.Object")) "byte")
                     ("getChar" ((obj "java.lang.Object")) "char")
                     ("getDouble" ((obj "java.lang.Object")) "double")
                     ("getFloat" ((obj "java.lang.Object")) "float")
                     ("getInt" ((obj "java.lang.Object")) "int")
                     ("getLong" ((obj "java.lang.Object")) "long")
                     ("getShort" ((obj "java.lang.Object")) "short")

                     ("set" ((obj "java.lang.Object") (value "java.lang.Object")) "void")
                     ("setBoolean" ((obj "java.lang.Object") (b "boolean")) "void")
                     ("setByte" ((obj "java.lang.Object") (b "byte")) "void")
                     ("setChar" ((obj "java.lang.Object") (c "char")) "void")
                     ("setDouble" ((obj "java.lang.Object") (d "double")) "void")
                     ("setFloat" ((obj "java.lang.Object") (f "float")) "void")
                     ("setInt" ((obj "java.lang.Object") ( i "int")) "void")
                     ("setLong" ((obj "java.lang.Object") (l "long")) "void")
                     ("setShort" ((obj "java.lang.Object") (s "short")) "void"))

  (def-jni-functions "java.lang.reflect.Constructor"
                     ("getParameterTypes" () "java.lang.Class<>")
                     ("newInstance" ((initargs "java.lang.Object<>")) "java.lang.Object"))

  (def-jni-functions "java.lang.reflect.Method"
                     ("getName" () "java.lang.String")
                     ("getParameterTypes" () "java.lang.Class<>")
                     ("getReturnType" () "java.lang.Class")
                     ("getModifiers" () "int")
                     ("invoke" ((object "java.lang.Object")
                                (args "java.lang.Object<>")) "java.lang.Object"
                               :raw-return t))

  (def-jni-functions "java.lang.reflect.Array"
                     ("get" ((array "java.lang.Object") (index "int")) "java.lang.Object" :static t)
                     ("getBoolean"
                      ((array "java.lang.Object") (index "int")) "boolean" :static t)
                     ("getByte"
                      ((array "java.lang.Object") (index "int")) "byte" :static t)
                     ("getChar"
                      ((array "java.lang.Object") (index "int")) "char" :static t)
                     ("getDouble"
                      ((array "java.lang.Object") (index "int")) "double" :static t)
                     ("getFloat"
                      ((array "java.lang.Object") (index "int")) "float" :static t)
                     ("getInt"
                      ((array "java.lang.Object") (index "int")) "int" :static t)
                     ("getShort"
                      ((array "java.lang.Object") (index "int")) "short" :static t)
                     ("getLong"
                      ((array "java.lang.Object") (index "int")) "long" :static t)
                     ("getLength" ((array "java.lang.Object")) "int" :static t)
                     ("newInstance" ((componentType "java.lang.Class")
                                     (length "int")) "java.lang.Object" :static t :overloaded t)
                     ("newInstance" ((componentType "java.lang.Class")
                                     (dimensions "int<>")) "java.lang.Object" :static t :overloaded t)
                     ("set" ((array "java.lang.Object") (index "int") (value "java.lang.Object"))
                            "void" :static t))


  (def-jni-function "java.lang.reflect.Modifier"
                    "isStatic" ((mod "int")) "boolean" :static t)

  (def-jni-constructor "java.lang.Boolean" ((value "boolean")))
  (def-jni-constructor "java.lang.Byte" ((value "byte")))
  (def-jni-constructor "java.lang.Character" ((value "char")))
  (def-jni-constructor "java.lang.Double" ((value "double")))
  (def-jni-constructor "java.lang.Float" ((value "float")))
  (def-jni-constructor "java.lang.Integer" ((value "int")))
  (def-jni-constructor "java.lang.Short" ((value "short")))

  (def-jni-function "java.lang.Boolean" "booleanValue" () "boolean")
  (def-jni-function "java.lang.Byte" "byteValue" () "byte")
  (def-jni-function "java.lang.Character" "charValue" () "char")
  (def-jni-function "java.lang.Double" "doubleValue" () "double")
  (def-jni-function "java.lang.Float" "floatValue" () "float")
  (def-jni-function "java.lang.Integer" "intValue" () "int")
  (def-jni-function "java.lang.Short" "shortValue" () "short")

  (def-jni-constructor "java.util.jar.JarFile" ((filename "java.lang.String")))
  (def-jni-function "java.util.jar.JarFile"
                    "entries" () "java.util.Enumeration")
  (def-jni-functions "java.util.Enumeration"
                     ("hasMoreElements" () "boolean")
                     ("nextElement" () "java.lang.Object"))
  (def-jni-functions "java.util.zip.ZipEntry"
                     ("isDirectory" () "boolean")
                     ("getName" () "java.lang.String"))


  (def-jni-functions "java.lang.Long"
                     ("valueOf" ((s "String")) "java.lang.Long" :static t)
                     ("intValue" () "int"))

  (def-jni-field "java.lang.Boolean" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Byte" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Character" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Float" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Integer" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Double" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Short" "TYPE" "Class" :static t)
  (def-jni-field "java.lang.Long" "TYPE" "Class" :static t)

  (def-jni-constructor "com.richhickey.jfli.LispInvocationHandler" ())
  (def-jni-function "java.lang.reflect.Proxy"
                    "newProxyInstance" ((loader "java.lang.ClassLoader")
                                        (interfaces "java.lang.Class<>")
                                        (h "InvocationHandler")) "java.lang.Object" :static t)

  (def-jni-function "java.lang.ClassLoader"
                    "getSystemClassLoader" () "ClassLoader" :static t)

  ) ;eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;; utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel)
  (defun ensure-package (name)
    "find the package or create it if it doesn't exist"
    (or (find-package name)
        (make-package name :use '())))
  (intern "Object" (ensure-package "java.lang"))
  (intern "String" (ensure-package "java.lang")))

(defmacro do-jarray ((x array) &body body)
  "jni-based, so not safe and not exported, but used by the implementation"
  (let ((gcount (gensym))
        (gi (gensym))
        (garray (gensym)))
    `(let* ((,garray ,array)
            (,gcount (jni:get-array-length ,garray)))
       (dotimes (,gi ,gcount)
         (let ((,x (jaref ,garray ,gi)))
           ,@body)))))


(defmacro doenum ((e enum) &body body)
  "jni-based, so not safe and not exported, but used by the implementation"
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
           ((not (enumeration.hasmoreelements ,genum)))
         (let ((,e (enumeration.nextelement ,genum)))
           ,@body)))))

;probably insufficiently general, works as used here
(defmacro get-or-init (place init-form)
  `(or ,place
       (setf ,place ,init-form)))

;from c.l.l.
(defmacro case-equal (exp &body clauses)
  (let ((temp (gensym)))
    `(let ((,temp ,exp))
       (cond ,@(mapcar #'(lambda (clause)
                           (destructuring-bind (keys . clause-forms) clause
                             (cond ((eq keys 'otherwise)
                                    `(t ,@clause-forms))
                                   (t
                                    (if (atom keys) (setq keys (list keys)))
                                    `((member ,temp ',keys :test #'equal)
                                      ,@clause-forms)))))
                       clauses)))))

;create object. to bootstrap the hierarchy
(defclass |java.lang|::object. ()
  ((ref :reader ref :initarg :ref)
   (lisp-allocated :reader lisp-allocated-p :initarg :lisp-allocated :initform nil))
  (:documentation "the superclass of all Java typed reference classes"))

(defun get-ref (x)
  "any function taking an object can be passed a raw java-ref ptr or a typed reference instance.
Will also convert strings for use as objects"
  (etypecase x
    (java-ref x)
    (|java.lang|::object. (ref x))
    (string (convert-to-java-string x))
    (null nil)))

(defun jeq (obj1 obj2)
  "are these 2 java objects the same object? Note that is not the same as Object.equals()"
  (is-same-object (get-ref obj1) (get-ref obj2)))


;;;;;;;;;;;;;;;;;;;;;;;; names and symbols ;;;;;;;;;;;;;;;;;;;;;;;
#|
The library does a lot with names and symbols, needing at various times to:
 - find stuff in Java - full names w/case required
 - create hopefully non-conflicting packages and member names

When you (def-java-class "java.lang.String") you get a bunch of symbols/names:
a package named '|java.lang|
a class-symbol '|java.lang|:STRING. (note the dot and case), 
   which can usually be used where a typename is required
   it also serves as the name of the Lisp typed reference class for string
   its symbol-value is the canonic-class-symbol (see below)
a canonic-class-symbol '|java.lang|::|String|
   can be used to reconstitute the full class name

I've started trying to flesh out the notion of a Java class designator, which can either be
the full class name as a string, the class-symbol, or one of :boolean, :int etc
|#

(defun canonic-class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|:|Object|"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern class (ensure-package package))))

(defun class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|:object."
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern (string-upcase (string-append class ".")) (ensure-package package))))

(defun java-class-name (class-sym)
  "inverse of class-symbol, only valid on class-syms created by def-java-class"
  (let ((canonic-class-symbol (symbol-value class-sym)))
    (string-append (package-name (symbol-package canonic-class-symbol))
                                                "."
                                                canonic-class-symbol)))

(defun member-symbol (full-class-name member-name)
  "members are defined case-insensitively in case-sensitive packages,
prefixed by 'classname.' -
(member-symbol \"java.lang.Object\" \"toString\") -> '|java.lang|::OBJECT.TOSTRING"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern (string-upcase (string-append class "." member-name)) (ensure-package package))))

(defun constructor-symbol (full-class-name)
  (member-symbol full-class-name "new"))

(defun get-java-class-ref (canonic-class-symbol)
  "class-ref is cached on the plist of the canonic class symbol"
  (get-or-init (get canonic-class-symbol :class-ref)
               (let ((class-name (string-append (package-name
                                                 (symbol-package canonic-class-symbol))
                                                "."
                                                canonic-class-symbol)))
                 (try-null (jni-find-class (nsubstitute #\/ #\. class-name))))))

(defun find-java-class (class-sym-or-string)
  "Given a Java class designator, returns the Java Class object."
  (ctypecase class-sym-or-string
    (symbol (case class-sym-or-string
              (:int integer.type)
              (:char character.type)
              (:long long.type)
              (:float float.type)
              (:boolean boolean.type)
              (:short short.type)
              (:double double.type)
              (:byte byte.type)
              (otherwise (get-java-class-ref class-sym-or-string))))
    (string (get-java-class-ref (canonic-class-symbol class-sym-or-string)))))

;;;;;;;;;;;;;;;;;;;;;; typed reference support ;;;;;;;;;;;;;;;;;;;;;;;;
#|
The library maintains a hierarchy of typed reference classes that parallel the
class hierarchy on the Java side
new returns a typed reference, but other functions that return objects
return raw references (for efficiency) 
make-typed-ref can create fully-typed wrappers when desired
|#

(defun get-superclass-names (full-class-name)
  (let* ((class (get-java-class-ref (canonic-class-symbol full-class-name)))
         (super (class.getsuperclass class))
         (interfaces (class.getinterfaces class))
         (supers ()))
    (do-jarray (i interfaces)
      (push i supers))
    ;hmmm - where should the base class go in the precedence list?
    ;is it more important than the interfaces? this says no
    (if super
        (push super supers)
      (push (find-java-class "java.lang.Object") supers))
    (setf supers (nreverse supers))
    ;now we need to fix up order so more derived classes are first
    ;but don't have a total ordering, so merge one at a time
    (let (result)
      (dolist (s supers)
        (setf result (merge 'list result (list s)
                            (lambda (x y)
                              (is-assignable-from x y)))))
      (mapcar #'class.getname result))))
#|
(defun get-superclass-names (full-class-name)
  (let* ((class (get-java-class-ref (canonic-class-symbol full-class-name)))
         (super (class.getsuperclass class))
         (interfaces (class.getinterfaces class))
         (supers ()))
    (do-jarray (i interfaces)
      (push (class.getname i) supers))
    ;hmmm - where should the base class go in the precedence list?
    ;is it more important than the interfaces? this says no
    (if super
        (push (class.getname super) supers)
      (push "java.lang.Object" supers))
    (nreverse supers)))
|#

(defun ensure-java-class (full-class-name)
  "walks the superclass hierarchy and makes sure all the classes are fully defined
(they may be undefined or just forward-referenced-class)
caches this has been done on the class-symbol's plist"
  (let* ((class-sym (class-symbol full-class-name))
         (class (find-class class-sym nil)))
    (if (or (eql class-sym '|java.lang|::object.)
            (get class-sym :ensured))
        class
      (let ((supers (get-superclass-names full-class-name)))
        (dolist (super supers)
          (ensure-java-class super))
        (unless (and class (subtypep class 'standard-object))
          (setf class
                (clos:ensure-class class-sym :direct-superclasses (mapcar #'class-symbol supers))))
        (setf (get class-sym :ensured) t)
        class))))

(defun ensure-java-hierarchy (class-sym)
  "Works off class-sym for efficient use in new
This will only work on class-syms created by def-java-class,
as it depends upon symbol-value being the canonic class symbol"
  (unless (get class-sym :ensured)
    (ensure-java-class (java-class-name class-sym))))

(defun make-typed-ref (java-ref)
  "Given a raw java-ref, determines the full type of the object
and returns an instance of a typed reference wrapper"
  (when java-ref
    (let ((class (object.getclass (get-ref java-ref))))
      (if (class.isarray class)
          (error "typed refs not supported for arrays (yet)")
        (make-instance (ensure-java-class (class.getname class)) :ref (get-ref java-ref) )))))


;;;;;;;;;;;;;;;;;;;;;;;;; Wrapper Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
In an effort to reduce the volume of stuff generated when wrapping entire libraries,
the wrappers just generate minimal stubs, which, if and when invoked at runtime,
complete the work of building thunking closures, so very little code is generated for
things never called (Java libraries have huge numbers of symbols).
Not sure if this approach matters, but that's how it works
|#

(defmacro def-java-class (full-class-name)
  "Given the package-qualified, case-correct name of a java class, will generate
wrapper functions for its constructors, fields and methods."
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (declare (ignore class))
    (let* ((class-sym (class-symbol full-class-name))
           (defs
            (list*
             `(ensure-package ,package)
          ;build a path from the simple class symbol to the canonic
             `(defconstant ,class-sym ',(canonic-class-symbol full-class-name))
             `(export ',class-sym (symbol-package ',class-sym))
             `(def-java-constructors ,full-class-name)
             `(def-java-methods ,full-class-name)
             `(def-java-fields ,full-class-name)
             (unless (string= full-class-name "java.lang.Object")
               (let ((supers (mapcar #'class-symbol (get-superclass-names full-class-name))))
                 (append (mapcar (lambda (p)
                                   `(ensure-package ,(package-name p)))
                                 (remove (symbol-package class-sym)
                                         (remove-duplicates (mapcar #'symbol-package supers))))
                         (list `(defclass ,(class-symbol full-class-name)
                                          ,supers ()))))))))
      `(locally ,@defs))))

(defun get-jar-classnames (jar-file-name &rest packages)
  "returns a list of strings, packages should be of the form \"java/lang\"
  for recursive lookup and \"java/util/\" for non-recursive"
  (let* ((jar (jarfile.new jar-file-name))
         (entries (jarfile.entries jar))
         (names ()))
    (doenum (e entries)
      (unless (zipentry.isdirectory e)
        (let ((ename (zipentry.getname e)))
          (flet ((matches (package)
                   (and (eql 0 (search package ename))
                        (or (not (eql #\/ (schar package (1- (length package))))) ;recursive
                            (not (find #\/ ename :start (length package))))))) ;non-subdirectory
            (when (and (eql (search ".class" ename)
                            (- (length ename) 6)) ;classname
                       ;don't grab anonymous inner classes
                       (not (and (find #\$ ename)
                                 (digit-char-p (schar ename (1+ (position #\$ ename))))))
                       (some #'matches packages))
              (push (nsubstitute #\. #\/ (subseq ename 0 (- (length ename) 6)))
                    names))))))
    names))

(defun dump-wrapper-defs-to-file (filename classnames)
  "given a list of classnames (say from get-jar-classnames), writes
calls to def-java-class to a file"
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (dolist (name (sort classnames #'string-lessp))
      (format s "(def-java-class ~S)~%" name))))

;;;;;;;;;;;;;;;;;;;;;;;;; constructors and new ;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

Every non-interface class with a public ctor will get;
  a constructor, classname.new
  a method defined on make-new, ultimately calling classname.new,
   specialized on (the value of) it's class-symbol (e.g. canonic sym)

Note that if the ctor is overloaded, there is just one function (taking a rest arg), 
which handles overload resolution

The new macro expands into a call to make-new
|#

(defgeneric make-new (class-sym &rest args)
  (:documentation "Allows for definition of before/after methods on ctors.
The new macro expands into call to this"))

(defun build-ctor-doc-string (name ctors)
  (with-output-to-string (s)
    (dolist (c ctors)
      (format s "~A(~{~#[~;~A~:;~A,~]~})~%"
              name
              (mapcar #'class-name-for-doc (jarray-to-list (constructor.getparametertypes c)))))))

(defmacro def-java-constructors (full-class-name)
"creates and exports a ctor func classname.new, defines a method of 
make-new specialized on the class-symbol"
  (let ((ctor-list (get-ctor-list full-class-name)))
    (when ctor-list
      (let ((ctor-sym (constructor-symbol full-class-name))
            (class-sym (class-symbol full-class-name)))
        `(locally
           (defun ,ctor-sym (&rest args)
             ,(build-ctor-doc-string full-class-name ctor-list)
             (apply #'install-constructors-and-call ,full-class-name args))
           (export ',ctor-sym (symbol-package ',ctor-sym))
           (defmethod make-new ((class-sym (eql ,class-sym)) &rest args)
             (apply (function ,ctor-sym) args)))))))

(defun get-ctor-list (full-class-name)
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (ctor-array (class.getconstructors class))
         (ctor-list (jarray-to-list ctor-array)))
    ctor-list))

(defun install-constructors-and-call (full-class-name &rest args)
  "initially the constructor symbol for a class is bound to this function,
when first called it will replace itself with the appropriate direct thunk,
then call the requested ctor - subsequent calls will be direct"
  (install-constructors full-class-name)
  (apply (constructor-symbol full-class-name) args))

(defun install-constructors (full-class-name)
  (let* ((ctor-list (get-ctor-list full-class-name)))
    (when ctor-list
      (setf (fdefinition (constructor-symbol full-class-name))
            (make-ctor-thunk ctor-list (class-symbol full-class-name))))))

(defun make-ctor-thunk (ctors class-sym)
  (if (rest ctors) ;overloaded
      (make-overloaded-ctor-thunk ctors class-sym)
    (make-non-overloaded-ctor-thunk (first ctors) class-sym)))

(defun make-non-overloaded-ctor-thunk (ctor class-sym)
  (let ((arg-boxers (get-arg-boxers (constructor.getparametertypes ctor))))
    (lambda (&rest args)
      (let ((arg-array (build-arg-array args arg-boxers)))
        (ensure-java-hierarchy class-sym)
        (prog1
            (make-instance class-sym
                           :ref (constructor.newinstance ctor arg-array)
                           :lisp-allocated t)
            ;(constructor.newinstance ctor arg-array)
          (when arg-array
            (delete-local-ref arg-array)))))))

(defun make-overloaded-ctor-thunk (ctors class-sym)
  (let ((thunks (make-ctor-thunks-by-args-length ctors class-sym)))
    (lambda (&rest args)
      (let ((fn (cdr (assoc (length args) thunks))))
        (if fn
            (apply fn
                   args)
          (error "invalid arity"))))))

(defun make-ctor-thunks-by-args-length (ctors class-sym)
  "returns an alist of thunks keyed by number of args"
  (let ((ctors-by-args-length (make-hash-table))
        (thunks-by-args-length nil))
    (dolist (ctor ctors)
      (let ((params-len (get-array-length (constructor.getparametertypes ctor))))
        (push ctor (gethash params-len ctors-by-args-length))))
    (maphash #'(lambda (args-len ctors)
                 (push (cons args-len
                             (if (rest ctors);truly overloaded
                                 (make-type-overloaded-ctor-thunk ctors class-sym)
                               ;only one ctor with this number of args
                               (make-non-overloaded-ctor-thunk (first ctors) class-sym)))
                       thunks-by-args-length))
             ctors-by-args-length)
    thunks-by-args-length))

(defun make-type-overloaded-ctor-thunk (ctors class-sym)
  "these methods have the same number of args and must be distinguished by type"
  (let ((thunks (mapcar #'(lambda (ctor)
                            (list (make-non-overloaded-ctor-thunk ctor class-sym)
                                  (jarray-to-list (constructor.getparametertypes ctor))))
                        ctors)))
    (lambda (&rest args)
      (block fn
        (let ((arg-types (get-types-of-args args)))
          (dolist (thunk-info thunks)
            (destructuring-bind (thunk param-types) thunk-info
              (when (is-congruent-type-list param-types arg-types)
                (return-from fn (apply thunk args)))))
          (error "No matching constructor"))))))

(defmacro new (class-spec &rest args)
"new class-spec args
class-spec -> class-name | (class-name this-name)
class-name -> \"package.qualified.ClassName\" | classname.
args -> [actual-arg]* [init-arg-spec]*
init-arg-spec -> init-arg | (init-arg)
init-arg -> :settable-field-or-method [params]* value ;note keyword
            | 
            .method-name [args]*                      ;note dot

Creates a new instance of class-name, using make-new generic function,
then initializes it by setting fields or accessors and/or calling member functions
If this-name is supplied it will be bound to the newly-allocated object and available
to the init-args"
  (labels ((mem-sym? (x)
             (or (keywordp x)
                 (and (symbolp x) (eql 0 (position #\. (symbol-name x))))))
           (mem-form? (x)
             (and (listp x) (mem-sym? (first x))))
           (mem-init? (x)
             (or (mem-sym? x) (mem-form? x)))
           (init-forms (x)
             (if x
                 (if (mem-form? (first x))
                     (cons (first x) (init-forms (rest x)))
                   (let ((more (member-if #'mem-init? (rest x))))
                     (cons (ldiff x more) (init-forms more)))))))
    (let* ((inits (member-if #'mem-init? args))
           (real-args (ldiff args inits))
           (class-atom (if (atom class-spec)
                           class-spec
                         (first class-spec)))
           (class-sym (if (symbolp class-atom)
                          ;(find-symbol (string-append (symbol-name class-atom) "."))
                          class-atom
                        (multiple-value-bind (package class) (jni::split-package-and-class class-atom)
                          (find-symbol (string-append (string-upcase class) ".") package))))
           (class-name (subseq (symbol-name class-sym) 0 (1- (length (symbol-name class-sym)))))
           (gthis (gensym)))
      (flet ((expand-init (x)
               (if (keywordp (first x)) ;setf field or property
                   `(setf (,(find-symbol (string-append class-name "." (symbol-name (first x))))
                           ,gthis ,@(butlast (rest x)))
                          ,@(last (rest x)))
                 ;.memfunc
                 `(,(find-symbol (string-append class-name (symbol-name (first x))))
                   ,gthis
                   ,@(rest x)))))
        `(let* ((,gthis (make-new ,class-sym ,@real-args))
                ,@(when (listp class-spec)
                    `((,(second class-spec) ,gthis))))
           ,@(mapcar #'expand-init (init-forms inits))
           ,gthis)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
all public fields will get a getter function classname.fieldname and a setter - (setf classname.fieldname)
instance fields take an first arg which is the instance
static fields also get a symbol-macro *classname.fieldname*
|#

(defmacro def-java-fields (full-class-name)
"fields will get a getter function classname.fieldname and a setter - (setf classname.fieldname)
instance fields take an first arg which is the instance
static fields also get a symbol-macro *classname.fieldname*"
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (fields (jarray-to-list (class.getfields class)))
         (defs nil))
    (dolist (field fields)
      (let* ((field-name (field.getname field))
             (field-sym (member-symbol full-class-name field-name))
             (is-static (modifier.isstatic (field.getmodifiers field))))
        (if is-static
            (let ((macsym (intern (string-append "*" (symbol-name field-sym) "*")
                                  (symbol-package field-sym))))
              (push `(defun ,field-sym ()
                       (install-static-field-and-get ,full-class-name ,field-name))
                    defs)
              (push `(defun (setf ,field-sym) (val)
                       (install-static-field-and-set ,full-class-name ,field-name val))
                    defs)
              (push `(export ',field-sym (symbol-package ',field-sym)) defs)
              (push `(define-symbol-macro ,macsym (,field-sym)) defs)
              (push `(export ',macsym (symbol-package ',macsym)) defs))
          (progn
            (push `(defun ,field-sym (obj)
                     (install-field-and-get ,full-class-name ,field-name obj))
                  defs)
            (push `(defun (setf ,field-sym) (val obj)
                     (install-field-and-set ,full-class-name ,field-name val obj))
                  defs)
            (push `(export ',field-sym (symbol-package ',field-sym)) defs)))))
    `(locally ,@(nreverse defs))))

(defun install-field-and-get (full-class-name field-name obj)
  (install-field full-class-name field-name)
  (funcall (member-symbol full-class-name field-name) obj))

(defun install-field-and-set (full-class-name field-name val obj)
  (install-field full-class-name field-name)
  (funcall (fdefinition `(setf ,(member-symbol full-class-name field-name))) val obj))

(defun install-static-field-and-get (full-class-name field-name)
  (install-field full-class-name field-name)
  (funcall (member-symbol full-class-name field-name)))

(defun install-static-field-and-set (full-class-name field-name val)
  (install-field full-class-name field-name)
  (funcall (fdefinition `(setf ,(member-symbol full-class-name field-name))) val))

(defun install-field (full-class-name field-name)
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (field (class.getfield class field-name))
         (field-sym (member-symbol full-class-name field-name))
         (is-static (modifier.isstatic (field.getmodifiers field)))
         (field-type-name (class.getname (field.gettype field)))
         (boxer (get-boxer-fn field-type-name))
         (unboxer (get-unboxer-fn field-type-name)))
    (if is-static
        (progn
          (setf (fdefinition field-sym)
                (lambda ()
                  (funcall unboxer (field.get field nil) t)))
          (setf (fdefinition `(setf ,field-sym))
                (lambda (arg)
                  (field.set field nil
                             (get-ref (if (and boxer (not (boxed? arg)))
                                          (funcall boxer arg)
                                        arg)))
                  arg)))
      (progn
        (setf (fdefinition field-sym)
              (lambda (obj)
                (funcall unboxer (field.get field (get-ref obj)) t)))
        (setf (fdefinition `(setf ,field-sym))
              (lambda (arg obj)
                (field.set field (get-ref obj)
                           (get-ref (if (and boxer (not (boxed? arg)))
                                        (funcall boxer arg)
                                      arg)))
                arg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
defines wrappers for all public methods of the class
As with ctors, if a method is overloaded a single wrapper is created that handles
overload resolution.
The wrappers have the name classname.methodname
If a method follows the JavaBeans property protocol (i.e. it is called getSomething or isSomething
and there is a corresponding setSomething, then a (setf classname.methodname) will be defined
that calls the latter
|#

(defun class-name-for-doc (class)
  (let ((name (class.getname class)))
    (if (class.isarray class)
        (decode-array-name name)
      name)))

(defun build-method-doc-string (name methods)
  (with-output-to-string (s)
    (dolist (m methods)
      (format s "~A~A ~A(~{~#[~;~A~:;~A,~]~})~%"
              (if (modifier.isstatic (method.getmodifiers m))
                  "static "
                "")
              (class.getname (method.getreturntype m))
              name
              (mapcar #'class-name-for-doc (jarray-to-list (method.getparametertypes m)))))))

(defmacro def-java-methods (full-class-name)
  (let ((methods-by-name (get-methods-by-name full-class-name))
        (defs nil))
    (maphash (lambda (name methods)
               (let ((method-sym (member-symbol full-class-name name)))
                 (push `(defun ,method-sym (&rest args)
                          ,(build-method-doc-string name methods)
                          (apply #'install-methods-and-call ,full-class-name ,name args))
                       defs)
                 (push `(export ',method-sym (symbol-package ',method-sym))
                       defs)
                 ;build setters when finding beans property protocol
                 (flet ((add-setter-if (prefix)
                          (when (eql 0 (search prefix name))
                            (let ((setname (string-append "set" (subseq name (length prefix)))))
                              (when (gethash setname methods-by-name)
                                (push `(defun (setf ,method-sym) (val &rest args)
                                         (progn
                                           (apply #',(member-symbol full-class-name setname)
                                                  (append args (list val)))
                                           val))
                                      defs))))))
                   (add-setter-if "get")
                   (add-setter-if "is"))))
             methods-by-name)
    `(locally ,@(nreverse defs))))

(defun install-methods-and-call (full-class-name method &rest args)
  "initially all the member function symbols for a class are bound to this function,
when first called it will replace them with the appropriate direct thunks,
then call the requested method - subsequent calls via those symbols will be direct"
  (install-methods full-class-name)
  (apply (member-symbol full-class-name method) args))

(defun decode-array-name (tn)
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
            (write-string (decode-array-name (subseq tn array-depth)) s)
            (dotimes (x array-depth)
              (write-string "[]" s))))))))

(defun jarray-to-list (array)
  (let (ret)
    (do-jarray (x array)
      (push x ret))
    (nreverse ret)))

(defun get-methods-by-name (full-class-name)
  "returns an #'equal hashtable of lists of java.lang.Method refs keyed by name"
  (let* ((class-sym (canonic-class-symbol full-class-name))
         (class (get-java-class-ref class-sym))
         (method-array (class.getmethods class))
         (methods-by-name (make-hash-table :test #'equal)))
    (do-jarray (method method-array)
      (push method (gethash (method.getName method) methods-by-name)))
    methods-by-name))

(defun install-methods (full-class-name)
  (let ((methods-by-name (get-methods-by-name full-class-name)))
    (maphash
     (lambda (name methods)
       (setf (fdefinition (member-symbol full-class-name name))
             (make-method-thunk methods)))
     methods-by-name)))

(defun make-method-thunk (methods)
  (if (rest methods) ;overloaded
      (make-overloaded-thunk methods)
    (make-non-overloaded-thunk (first methods))))

(defun make-non-overloaded-thunk (method)
  (let ((unboxer-fn (get-unboxer-fn (class.getname (method.getreturntype method))))
        (arg-boxers (get-arg-boxers (method.getparametertypes method)))
        (is-static (modifier.isstatic (method.getmodifiers method))))
    (lambda (&rest args)
      (let ((arg-array (build-arg-array (if is-static args (rest args)) arg-boxers)))
        (prog1
            (funcall unboxer-fn
                     (method.invoke method
                                    (if is-static nil (get-ref (first args)))
                                    arg-array) t)
          (when arg-array
            (delete-local-ref arg-array)))))))

(defun make-overloaded-thunk (methods)
  (let ((thunks (make-thunks-by-args-length methods)))
    (lambda (&rest args)
      (let ((fn (cdr (assoc (length args) thunks))))
        (if fn
            (apply fn
                   args)
          (error "invalid arity"))))))

(defun make-thunks-by-args-length (methods)
  "returns an alist of thunks keyed by number of args"
  (let ((methods-by-args-length (make-hash-table))
        (thunks-by-args-length nil))
    (dolist (method methods)
      (let ((is-static (modifier.isstatic (method.getmodifiers method)))
            (params-len (get-array-length (method.getparametertypes method))))
        (push method (gethash (if is-static params-len (1+ params-len))
                              methods-by-args-length))))
    (maphash #'(lambda (args-len methods)
                 (push (cons args-len
                             (if (rest methods);truly overloaded
                                 (make-type-overloaded-thunk methods)
                               ;only one method with this number of args
                               (make-non-overloaded-thunk (first methods))))
                       thunks-by-args-length))
             methods-by-args-length)
    thunks-by-args-length))

(defun make-type-overloaded-thunk (methods)
  "these methods have the same number of args and must be distinguished by type"
  (let ((thunks (mapcar #'(lambda (method)
                            (list (make-non-overloaded-thunk method)
                                  (modifier.isstatic (method.getmodifiers method))
                                  (jarray-to-list (method.getparametertypes method))))
                        methods)))
    (lambda (&rest args)
      (block fn
        (let ((arg-types (get-types-of-args args)))
          (dolist (thunk-info thunks)
            (destructuring-bind (thunk is-static param-types) thunk-info
              (when (is-congruent-type-list param-types (if is-static arg-types (rest arg-types)))
                (return-from fn (apply thunk args)))))
          (error "No matching method"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; array support ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jref (array &rest subscripts)
  "like aref, for Java arrays of non-primitive/reference types, settable"
  (assert (every #'integerp subscripts))
  (do*
       ((sub subscripts (rest sub))
        (a (get-ref array) (get-ref (array.get a (first sub)))))
       ((null (rest sub))
        (array.get a (first sub)))))

(defun (setf jref) (val array &rest subscripts)
  (assert (every #'integerp subscripts))
  (do*
       ((sub subscripts (rest sub))
        (a (get-ref array) (get-ref (array.get a (first sub)))))
       ((null (rest sub))
        (array.set a (first sub) (get-ref val))
        val)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro def-refs (&rest types)
    `(locally
       ,@(mapcan
          (lambda (type)
            (let ((ref-sym (intern (string-upcase (string-append "jref-" (symbol-name type))))))
              (list 
               `(defun ,ref-sym (array &rest subscripts)
                  ,(format nil "like aref, for Java arrays of ~A, settable" (symbol-name type))
                  (assert (every #'integerp subscripts))
                  (do*
                       ((sub subscripts (rest sub))
                        (a (get-ref array) (get-ref (array.get a (first sub)))))
                       ((null (rest sub))
                        (,(intern (string-upcase (string-append "array.get" (symbol-name type))))
                         a (first sub)))))

               `(defun (setf ,ref-sym) (val array &rest subscripts)
                  (assert (every #'integerp subscripts))
                  (do*
                       ((sub subscripts (rest sub))
                        (a (get-ref array) (get-ref (array.get a (first sub)))))
                       ((null (rest sub))
                        (array.set a (first sub)
                                   (,(intern (string-upcase (string-append "box-"
                                                                           (symbol-name type))))
                                    val))
                        val))))))
          types))))

;arrays of primitives have their own accessors
(def-refs boolean byte char double float int short long)

(defun jlength (array)
  "like length, for Java arrays"
  (array.getlength (get-ref array)))

(defgeneric make-new-array (type &rest dimensions)
  (:documentation "generic function, with methods for all Java class designators")
  (:method (type &rest dims)
   (assert (every #'integerp dims))
   (if (rest dims)
       (let* ((ndim (length dims))
              (dim-array (new-int-array ndim)))
         (dotimes (i ndim)
           (array.set dim-array i (box-int (nth i dims))))
         (array.newinstance<java.lang.class-int<>> type dim-array))
     (array.newinstance<java.lang.class-int> type (first dims)))))

(defmethod make-new-array ((type symbol) &rest dimensions)
  (apply #'make-new-array (get-java-class-ref type) dimensions))

(defmethod make-new-array ((type string) &rest dimensions)
  (apply #'make-new-array (find-java-class type) dimensions))

(defmethod make-new-array ((type (eql :char)) &rest dimensions)
  (apply #'make-new-array character.type dimensions))

(defmethod make-new-array ((type (eql :int)) &rest dimensions)
  (apply #'make-new-array integer.type dimensions))

(defmethod make-new-array ((type (eql :boolean)) &rest dimensions)
  (apply #'make-new-array boolean.type dimensions))

(defmethod make-new-array ((type (eql :double)) &rest dimensions)
  (apply #'make-new-array double.type dimensions))

(defmethod make-new-array ((type (eql :byte)) &rest dimensions)
  (apply #'make-new-array byte.type dimensions))

(defmethod make-new-array ((type (eql :float)) &rest dimensions)
  (apply #'make-new-array float.type dimensions))

(defmethod make-new-array ((type (eql :short)) &rest dimensions)
  (apply #'make-new-array short.type dimensions))

(defmethod make-new-array ((type (eql :long)) &rest dimensions)
  (apply #'make-new-array long.type dimensions))

;;;;;;;;;;;;;;;;;;;;;;;;;; arg/param helpers ;;;;;;;;;;;;;;;;;;;;;;

(defun get-arg-boxers (param-types)
  "returns a list with one entry per param, either nil or a function that boxes the arg"
  (let (ret)
    (do-jarray (param-type param-types)
      (push (get-boxer-fn (class.getname param-type))
            ret))
    (nreverse ret)))

(defun build-arg-array (args arg-boxers)
  (when args
    (let* ((arg-array (new-object-array (length args)
                                      ;duplication of class-symbol logic
                                      ;but must be fast
                                        (get-java-class-ref '|java.lang|::|Object|)
                                        nil)))
      (do ((i 0 (incf i))
           (args args (rest args))
           (boxers arg-boxers (rest boxers)))
          ((null args))
        (let ((arg (first args))
              (boxer (first boxers)))
          (setf (jaref arg-array i)
                (get-ref (if (and boxer (not (boxed? arg)))
                             (funcall boxer arg)
                           arg)))))
      arg-array)))

(defun get-types-of-args (args)
  (let (ret)
    (dolist (arg args)
      (push (infer-box-type arg)
            ret))
    (nreverse ret)))

(defun is-congruent-type-list (param-types arg-types)
  (every #'(lambda (arg-type param-type)
             (if arg-type
                 (is-assignable-from arg-type param-type)
               ;nil was passed - must be boolean or non-primitive target type
               (or (not (class.isprimitive param-type))
                   (is-assignable-from boolean.type param-type))))
         arg-types param-types))


;;;;;;;;;;;;;;;;;;;;;;;; argument conversion and boxing ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun box-string (s)
"Given a string or symbol, returns reference to a Java string"
  (local-ref-to-global-ref (convert-to-java-string s)))

(defun unbox-string (ref &optional delete-local)
  "Given a reference to a Java string, returns a Lisp string" 
  (declare (ignore delete-local))
  (convert-from-java-string (get-ref ref)))

(defun get-boxer-fn (class-name)
  (case-equal class-name
    ("int" #'box-int)
    ("boolean" #'box-boolean)
    ("double" #'box-double)
    ("java.lang.String" #'convert-to-java-string)
    ("char" #'box-char)
    ("byte" #'box-byte)
    ("float" #'box-float)
    ("long" #'box-long)
    ("short" #'box-short)
    (otherwise nil)))

(defun get-boxer-fn-sym (class-name)
  (case-equal class-name
    ("int" 'box-int)
    ("boolean" 'box-boolean)
    ("double" 'box-double)
    ("java.lang.String" 'convert-to-java-string)
    ("char" 'box-char)
    ("byte" 'box-byte)
    ("float" 'box-float)
    ("long" 'box-long)
    ("short" 'box-short)
    ("void" 'box-void)
    (otherwise 'identity)))

(defun boxed? (x)
  (or (java-ref-p x)
      (typep x '|java.lang|::object.)))

(defun infer-box-type (x)
  (cond
   ((null x) nil)
   ((boxed? x) (object.getclass (get-ref x)))
   ((integerp x) integer.type)
   ((numberp x) double.type)
   ((eq x t) boolean.type)
   ((or (stringp x) (symbolp x))
    (get-java-class-ref '|java.lang|::|String|))
   (t (error "can't infer box type"))))

(defun get-unboxer-fn (class-name)
  (case-equal class-name
    ("int" #'unbox-int)
    ("boolean" #'unbox-boolean)
    ("double" #'unbox-double)
    ("java.lang.String" #'unbox-string)
    ("void" #'unbox-void)
    ("char" #'unbox-char)
    ("byte" #'unbox-byte)
    ("float" #'unbox-float)
    ("long" #'unbox-long)
    ("short" #'unbox-short)
    (otherwise  #'unbox-ref)))

(defun get-unboxer-fn-sym (class-name)
  (case-equal class-name
    ("int" 'unbox-int)
    ("boolean" 'unbox-boolean)
    ("double" 'unbox-double)
    ("java.lang.String" 'unbox-string)
    ("void" 'unbox-void)
    ("char" 'unbox-char)
    ("byte" 'unbox-byte)
    ("float" 'unbox-float)
    ("long" 'unbox-long)
    ("short" 'unbox-short)
    (otherwise  'unbox-ref)))

(defun unbox-ref (x &optional delete-local)
  (declare (ignore delete-local))
  (local-ref-to-global-ref x))

(defun unbox-void (x &optional delete-local)
  (declare (ignore x delete-local))
  nil)

(defun box-void (x)
  (declare (ignore x))
  nil)

(defun box-boolean (x)
  (boolean.new x))

(defun unbox-boolean (obj &optional delete-local)
  (prog1
      (boolean.booleanvalue (get-ref obj))
    (when delete-local (delete-local-ref obj))))

(defun box-byte (x)
  (assert (integerp x))
  (byte.new x))

(defun unbox-byte (x &optional delete-local)
  (prog1
      (byte.bytevalue (get-ref x))
    (when delete-local (delete-local-ref x))))

(defun box-char (x)
  (character.new x))

(defun unbox-char (x &optional delete-local)
  (prog1
      (character.charvalue (get-ref x))
    (when delete-local (delete-local-ref x))))

(defun box-double (x)
  (assert (floatp x))
  (double.new (coerce x 'double-float)))

(defun unbox-double (x &optional delete-local)
  (prog1
      (double.doublevalue (get-ref x))
    (when delete-local (delete-local-ref x))))

(defun box-float (x)
  (assert (floatp x))
  (float.new x))

(defun unbox-float (x &optional delete-local)
  (prog1
      (float.floatvalue (get-ref x))
    (when delete-local (delete-local-ref x))))

(defun box-int (x)
  (assert (integerp x))
  (integer.new x))

(defun unbox-int (x &optional delete-local)
  (prog1
      (integer.intvalue (get-ref x))
    (when delete-local (delete-local-ref x))))

;can't directly construct Long because LW doesn't support long long fli on 32 bit platforms
(defun box-long (x)
  (assert (integerp x))
  (long.valueof (princ-to-string x)))

;here too, can only get an ints worth - aargh
(defun unbox-long (obj &optional delete-local)
  (prog1
      (parse-integer (object.tostring (get-ref obj)))
    (when delete-local (delete-local-ref obj))))

(defun box-short (x)
  (assert (integerp x))
  (short.new x))

(defun unbox-short (x &optional delete-local)
  (prog1
      (short.shortvalue (get-ref x))
    (when delete-local (delete-local-ref x))))


;;;;;;;;;;;;;;;;;;;;;;;; proxy support ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proxy-hashcode (proxy)
  ;use the hashcode of the proxy's class, 
  ;because hashcode() on the proxy flows through to the invocation handler
  ;is this rem guaranteed to be a fixnum?
  (rem (object.hashcode (object.getclass proxy)) most-positive-fixnum))

(defvar *proxy-table* (make-hash-table :test #'jeq :hash-function #'proxy-hashcode))

;(defvar *proxy-list* nil)

(defun store-proxy (proxy method-fn-alist)
  ;(push (cons proxy method-fn-alist) *proxy-list*)
  (setf (gethash proxy *proxy-table*) method-fn-alist))

(defun recall-proxy (proxy)
  ;(cdr (assoc proxy *proxy-list* :test #'jeq))
  (gethash proxy *proxy-table*))

(defun unregister-proxy (proxy)
"Stops handling for the proxy and removes references from the Lisp side.
Make sure it is no longer referenced from Java first!"
  (remhash proxy *proxy-table*))

(defun invocation-handler (proxy method args)
  (let* ((method-fn-alist (recall-proxy proxy))
         (fn (and method-fn-alist (second (assoc (object.tostring method) method-fn-alist
                                                 :test #'equal)))))
    (if fn
        (funcall fn args)
      (progn
        ;(throw-new  (find-java-class "java.lang.UnsupportedOperationException")
        ;            "No function registered in Lisp proxy object")
        nil))))

(defun enable-java-proxies ()
  "must be called before any call to new-proxy, and requires jfli.jar be in the classpath"
  (jni:register-invocation-handler #'invocation-handler))

(defun make-proxy-instance (&rest interface-defs)
  (let* ((interfaces (mapcar #'first interface-defs))
         (method-fn-alist (mapcan #'second interface-defs))
         (len (length interfaces))
         (iarray (array.newinstance<java.lang.class-int> (get-java-class-ref '|java.lang|::|Class|)
                                                         len)))
    (dotimes (x len)
      (setf (jref iarray x) (nth x interfaces)))
    (let ((proxy (proxy.newproxyinstance (classloader.getsystemclassloader)
                                         iarray
                                         (lispinvocationhandler.new))))
      (store-proxy proxy method-fn-alist)
      proxy)))

(defun find-java-class-in-macro (name)
  (find-java-class
   (if (symbolp name)
       (symbol-value name)
     name)))

(defmacro new-proxy (&rest interface-defs)
"interface-def -> (interface-name method-defs+)
interface-name -> \"package.qualified.ClassName\" | classname. (must name a Java interface type)
method-def -> (method-name arg-defs* body)
arg-def -> arg-name | (arg-name arg-type)
arg-type -> \"package.qualified.ClassName\" | classname. | :primitive
method-name -> symbol | string (matched case-insensitively)

Creates, registers and returns a Java object that implements the supplied interfaces"

  (labels ((process-idefs (idefs)
             (when idefs
               (cons (process-idef (first idefs))
                     (process-idefs (rest idefs)))))
           (process-idef (idef)
             (destructuring-bind (interface-name &rest method-defs) idef
               (let* ((methods (class.getmethods (find-java-class-in-macro interface-name)))
                      (ret `(list (find-java-class ,interface-name)
                                  (list ,@(mapcar (lambda (method-def)
                                                    (process-method-def method-def methods))
                                                  method-defs)))))
                 ;check to make sure every function is defined
                 (do-jarray (method methods)
                   (let ((mname (object.tostring method)))
                     (unless (member mname (rest (third ret)) :key #'second :test #'equal)
                       (warn (format nil "proxy doesn't define:~%~A" mname)))))
                 ret)))
           (process-method-def (method-def methods)
             (destructuring-bind (method-name (&rest arg-defs) &body body) method-def
               (let ((method (matching-method method-name arg-defs methods))
                     (gargs (gensym)))
                 `(list ,(object.tostring method)
                        (lambda (,gargs)
                          (,(get-boxer-fn-sym (class.getname (method.getreturntype method)))
                           (let ,(arg-lets arg-defs
                                           (jarray-to-list (method.getparametertypes method))
                                           gargs
                                           0)
                             ,@body)))))))
           (arg-lets (arg-defs params gargs idx)
             (when arg-defs
               (let ((arg (first arg-defs))
                     (param (first params)))
                 (cons `(,(if (atom arg) arg (first arg))
                         (,(get-unboxer-fn-sym (class.getname param))
                          (jref ,gargs ,idx) t))
                       (arg-lets (rest arg-defs) (rest params) gargs (1+ idx))))))
           (matching-method (method-name arg-defs methods)
             (let (match)
               (do-jarray (method methods)
                 (when (method-matches method-name arg-defs method)
                   (if match
                       (error (format nil "more than one method matches ~A" method-name))
                     (setf match method))))
               (or match (error (format nil "no method matches ~A" method-name)))))
           (method-matches (method-name arg-defs method)
             (when (string-equal method-name (method.getname method))
               (let ((params (method.getparametertypes method)))
                 (when (= (length arg-defs) (jlength params))
                   (is-congruent arg-defs params)))))
           (is-congruent (arg-defs params)
             (every (lambda (arg param)
                      (or (atom arg) ;no type spec matches anything
                          (jeq (find-java-class-in-macro (second arg)) param)))
                    arg-defs (jarray-to-list params))))
    `(make-proxy-instance ,@(process-idefs interface-defs))))
