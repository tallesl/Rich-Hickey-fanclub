;    Copyright (c) Rich Hickey. All rights reserved.
;    The use and distribution terms for this software are covered by the
;    Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;    which can be found in the file CPL.TXT at the root of this distribution.
;    By using this software in any fashion, you are agreeing to be bound by
;    the terms of this license.
;    You must not remove this notice, or any other, from this software.

(defpackage :foil
  (:use :common-lisp)
  (:export 

   :get-library-classnames
   :dump-wrapper-defs
   :dump-wrapper-defs-to-file
   :def-foil-class
   :full-class-name

   :equals
   :instance-of
   :to-string
   :hash
   :marshall
   :get-type
   :ensure-typed-ref

   :get-type-for-name

   :ensure-package

   :new
   :make-new
   :make-new-vector
   :vref
   :vlength
   :box
   :box-vector

   :iref

   :new-proxy
   :make-new-proxy
   :handle-proxy-call
   
   :with-vm
   :with-vm-of

   :with-marshalling
   :*marshalling-depth*
   :*marshalling-flags*
   :+MARSHALL-NO-IDS+
   :+MARSHALL-ID+
   :+MARSHALL-HASH+
   :+MARSHALL-TYPE+

   :fref
   :fref-vm
   :fref-id
   :fref-hash
   :fref-type
   :fref-val

   :*fvm*
   :*thread-fvm*
   :*thread-fvm-stream*
   :foreign-vm

   ))

(in-package :foil)

;; porting section - hopefully everything LW-specific is here and the rest is straight CL
;; find equivalents for your CL

(defun string-append (&rest strings)
  #+:lispworks(apply #'lispworks::string-append strings)
  #-:lispworks
  (apply #'concatenate 'string 
	 (mapcar #'(lambda (s) (if (symbolp s) (symbol-name s) s)) strings)))

(defun add-special-free-action (fsym)
  #+:lispworks(hcl:add-special-free-action fsym))

(defun flag-special-free-action (obj)
  #+:lispworks(hcl:flag-special-free-action obj))

(defun make-value-weak-hash-table ()
  #+:lispworks(make-hash-table :weak-kind :value)
  #+cmu (make-hash-table :weak-p t)
  #+sbcl (make-hash-table :weak-p nil)
  #+allegro (make-hash-table :values :weak))


(eval-when (:compile-toplevel :load-toplevel)
  (setf (fdefinition 'ensure-class) 
        #+lispworks #'clos:ensure-class
        #+cmu #'pcl:ensure-class
        #+sbcl #'sb-mop:ensure-class
        #+allegro #'mop:ensure-class))
 

;;;;; end porting section ;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *fvm* nil
  "all messages will be sent to the foreign-vm to which this is bound")

(defvar *thread-fvm-stream* nil
  "if this thread is waiting on a callback channel, and (eql *fvm* *thread-fvm*), use this stream")

(defvar *thread-fvm* nil
  "if, set, this thread is waiting on a callback from this vm")


#|
please ignore this scratchpad stuff
(load "/dev/foil/foil")
(use-package :foil)
(require "comm")
(setf *fvm* (make-instance 'foreign-vm
                           :stream
                           (comm:open-tcp-stream "localhost" 13579)))

|#

(defconstant +MARSHALL-NO-IDS+ 0)
(defconstant +MARSHALL-ID+ 1)
(defconstant +MARSHALL-TYPE+ 2)
(defconstant +MARSHALL-HASH+ 4)

(defconstant +CALLABLE-METHOD+ 0)
(defconstant +CALLABLE-FIELD+ 1)
(defconstant +CALLABLE-PROPERTY-GET+ 3)
(defconstant +CALLABLE-PROPERTY-SET+ 4)

(defvar *marshalling-flags* +marshall-id+)
(defvar *marshalling-depth* 0)

(defvar *in-async-callback* nil)

(defvar *foil-readtable* 
  (let* ((tbl (copy-readtable nil))
         (*readtable* tbl))
    (set-macro-character #\} (get-macro-character #\)))
    (set-dispatch-macro-character #\# #\{
                                  (lambda (stream c1 c2)
                                    (declare (ignore c1 c2))
                                    (apply #'handle-braces-macro (read-delimited-list #\} stream t))))
    tbl))

(defclass fref ()
  ((vm :reader fref-vm :initarg :vm )
  (id :reader fref-id :initarg :id)
  (rev :accessor fref-rev :initarg :rev)
  (type :accessor fref-type :initarg :type)
  (hash :accessor fref-hash :initarg :hash)
  (val :accessor fref-val :initarg :val))
  (:default-initargs :vm *fvm* :type nil :hash nil :val nil))

(defun make-fref (id rev &key type hash val)
  (let ((ret (make-instance 'fref :id id :rev rev :type type :hash hash :val val)))
    #+lispworks (flag-special-free-action ret)
    #+cmu (ext:finalize ret #'foil::free-fref)
    #+sbcl (sb-ext:finalize ret #'foil::free-fref)
    #+allegro (excl:schedule-finalization ret #'foil::free-fref)
    ret))

(defun free-fref (obj)
  (when (typep obj 'fref)
    (push obj (fvm-free-list (fref-vm obj)))))

#+lispworks 
 (add-special-free-action 'foil::free-fref)

(defmethod print-object ((fref fref) stream)
  (format stream "#}~A" (fref-id fref)))

(defclass foreign-vm ()
  ((stream :initarg :stream :reader fvm-stream)
   (fref-table :initform (make-value-weak-hash-table) :reader fvm-fref-table)
   (symbol-table :initform (make-hash-table) :reader fvm-symbol-table)
   (free-list :initform nil :accessor fvm-free-list)))

(defun get-fvm-stream ()
  (if (and *thread-fvm-stream* (eq *fvm* *thread-fvm*))
      *thread-fvm-stream*
    (fvm-stream *fvm*)))

(defun send-message (&rest args)
  (with-standard-io-syntax
    (let* ((send-stream (get-fvm-stream))
           (free-list (fvm-free-list *fvm*)))
      (when free-list
        (setf (fvm-free-list *fvm*) nil)
        (free-frefs send-stream free-list))
      (format send-stream "~S" args)
      (force-output send-stream)
      (process-return-message))))

(defun free-frefs (strm frefs)
  (format strm "(:free")
  (dolist (fref frefs)
    (format strm " ~S ~S" (fref-id fref) (fref-rev fref)))
  (format strm ")")
  (force-output strm)
  (process-return-message))

(defun process-return-message ()
  (let* ((*readtable* *foil-readtable*)
         (ret-stream (get-fvm-stream))
         (done nil)
         (ret nil))
    (do ()
        (done ret)
      (let ((msg (read ret-stream)))
         (case (first msg)
           (:ret (setf done t ret (second msg)))
           (:err (error (third msg))) ;just dump stack trace for now
           (:proxy-call
            (multiple-value-bind (x cond)
                (ignore-errors
                  (format ret-stream "(:ret ~S)" (apply #'handle-proxy-call (rest msg))))
              (declare (ignore x))
              (when cond
                (format ret-stream "(:err ~S)" (format nil "Error during proxy call: ~A~%" cond))))
            (force-output ret-stream)))))))
#|
(defun process-return-message ()
  (let* ((*readtable* *foil-readtable*)
         (ret-stream (get-fvm-stream))
         (msg (read ret-stream)))
    (case (first msg)
      (:ret (second msg))
      (:err (error (third msg))) ;just dump stack trace for now
      ;nested call, hopefully will get TCO
      (:proxy-call
       (format ret-stream "(:ret ~S)" (apply #'handle-proxy-call (rest msg)))
       (force-output ret-stream)
       (process-return-message)))))
|#

(defun handle-braces-macro (cmd &rest args)
  (ecase cmd
    (:ref (apply #'register-fref args))))

;probably insufficiently general, works as used here
(defmacro get-or-init (place init-form)
  `(or ,place
       (setf ,place ,init-form)))

(defun lookup-fref (id)
  (gethash id (fvm-fref-table *fvm*)))

(defun register-fref (id rev &key type hash val)
  (if id
      (let ((fref (get-or-init (gethash id (fvm-fref-table *fvm*))
                               (make-fref id rev))))
        (setf (fref-rev fref) rev)
        (when type
          (setf (fref-type fref) type)
          (ensure-typed-ref fref))
        (when hash
          (setf (fref-hash fref) hash))
        (when val
          (setf (fref-val fref) val))
        fref)
    val))

(eval-when (:compile-toplevel :load-toplevel)
  (defun ensure-package (name)
    "find the package or create it if it doesn't exist"
    (or (find-package name)
        (make-package name :use '()))))


(defun type-arg (arg)
  (etypecase arg
    (keyword arg)
    (string arg)
    (fref arg)
    (symbol (find-class-ref arg))))

;;;;;;;;;;;;;;;;;;;;;;;; names and symbols ;;;;;;;;;;;;;;;;;;;;;;;
#|
The library does a lot with names and symbols, needing at various times to:
 - find stuff in Java/CLR - full names w/case required
 - create hopefully non-conflicting packages and member names

When you (def-foil-class "java.lang.String") you get a bunch of symbols/names:
a package named '|java.lang|
a class-symbol '|java.lang|:STRING. (note the dot and case), 
   which can usually be used where a typename is required
   it also serves as the name of the Lisp typed reference class for string
   its symbol-value is the canonic-class-symbol (see below)
a canonic-class-symbol '|java.lang|::|String|
   can be used to reconstitute the full class name

I've started trying to flesh out the notion of a foil class designator, which can either be
the full class name as a string, the class-symbol, or one of :boolean, :int etc
|#


(defun split-package-and-class (name)
    (let ((p (position #\. name :from-end t)))
      (unless p (error "must supply package-qualified classname"))
      (values (subseq name 0 p)
              (subseq name (1+ p)))))

(defun canonic-class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|:|Object|"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern class (ensure-package package))))

(defun class-symbol (full-class-name)
  "(\"java.lang.Object\") -> '|java.lang|:object."
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern (string-upcase (string-append class ".")) (ensure-package package))))

(defun foil-class-name (class-sym)
  "inverse of class-symbol, only valid on class-syms created by def-foil-class"
  (let ((canonic-class-symbol (symbol-value class-sym)))
    (full-class-name canonic-class-symbol)))

(defun full-class-name (canonic-class-symbol)
  "inverse of canonic-class-symbol"
  (string-append (package-name (symbol-package canonic-class-symbol))
                                                "."
                                                canonic-class-symbol))

(defun member-symbol (full-class-name member-name)
  "members are defined case-insensitively in case-sensitive packages,
prefixed by 'classname.' -
(member-symbol \"java.lang.Object\" \"toString\") -> '|java.lang|::OBJECT.TOSTRING"
  (multiple-value-bind (package class) (split-package-and-class full-class-name)
    (intern (string-upcase (string-append class "." member-name)) (ensure-package package))))

(defun constructor-symbol (full-class-name)
  (member-symbol full-class-name "new"))


;;;;;;;;;;;;;;;;;;;;;; typed reference support ;;;;;;;;;;;;;;;;;;;;;;;;
#|
The library maintains a hierarchy of typed reference classes that parallel the
class hierarchy on the foreign VM side
new returns a typed reference, but other functions that return objects
return raw references (for efficiency) 
ensure-typed-ref can create fully-typed wrappers when desired
|#

(defun get-superclass-names (full-class-name)
  (send-message :bases full-class-name))

(defun ensure-foil-class (full-class-name)
  "walks the superclass hierarchy and makes sure all the classes are fully defined
(they may be undefined or just forward-referenced-class)
caches this has been done on the class-symbol's plist"
  (let* ((class-sym (class-symbol full-class-name))
         (class (find-class class-sym nil)))
    (if (get class-sym :ensured)
        class
      (let ((supers (get-superclass-names full-class-name)))
        (mapc #'ensure-foil-class supers)
        (unless (and class (subtypep class 'standard-object))
          (setf class
                (ensure-class class-sym
                              :direct-superclasses (if supers
                                                       (mapcar #'class-symbol supers)
                                                     '(fref)))))
        (setf (get class-sym :ensured) t)
        class))))

(defun ensure-foil-hierarchy (class-sym)
  "Works off class-sym for efficient use in new
This will only work on class-syms created by def-foil-class
as it depends upon symbol-value being the canonic class symbol"
  (unless (get class-sym :ensured)
    (ensure-foil-class (foil-class-name class-sym))))

(defun get-type (fref)
  (get-or-init (fref-type fref)
               (send-message :type-of fref)))

(defun get-type-for-name (full-class-name)
  (find-class-ref (canonic-class-symbol full-class-name)))

(defun find-class-ref (class-sym)
  (get-or-init (gethash class-sym (fvm-symbol-table *fvm*))
                            (send-message :tref (full-class-name class-sym))))

(defun find-method-ref (class-sym name method-sym)
  (get-or-init (gethash method-sym (fvm-symbol-table *fvm*))
                            (send-message :cref +callable-method+ (find-class-ref class-sym) name)))

(defun find-prop-get (class-sym name method-sym)
  (get-or-init (gethash method-sym (fvm-symbol-table *fvm*))
                            (send-message :cref +callable-property-get+ (find-class-ref class-sym) name)))

(defun find-prop-set (class-sym name method-sym)
  (get-or-init (gethash method-sym (fvm-symbol-table *fvm*))
                            (send-message :cref +callable-property-set+ (find-class-ref class-sym) name)))

(defun find-field-ref (class-sym name field-sym)
  (get-or-init (gethash field-sym (fvm-symbol-table *fvm*))
                            (send-message :cref +callable-field+ (find-class-ref class-sym) name)))

(defun class-name-of (fref)
  (fref-val (get-type fref)))

(defun ensure-typed-ref (fref)
  "Given a raw fref, determines the full type of the object
and change-classes to a typed reference wrapper"
  (when (and fref (eql (find-class 'fref) (class-of fref)))
    (let ((class-name (class-name-of fref)))
      ;(when (class.isarray class)
      ;    (error "typed refs not supported for arrays (yet)")
      (change-class fref (ensure-foil-class class-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Wrapper Generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-tag (tag tagged-alist)
  (rest (assoc tag tagged-alist)))

(defun find-tag-atom (tag tagged-alist)
  (car (find-tag tag tagged-alist)))

(defun do-def-foil-class (full-class-name)
  (let ((bases (send-message :bases full-class-name))
        (members (send-message :members full-class-name)))
    (multiple-value-bind (package class) (split-package-and-class full-class-name)
      (declare (ignore class))
      (let* ((class-sym (class-symbol full-class-name))
             (canonic-class-sym (canonic-class-symbol full-class-name))
             (defs
              (list*
               `(ensure-package ,package)
          ;build a path from the simple class symbol to the canonic
               `(defconstant ,class-sym ',(canonic-class-symbol full-class-name))
               `(export ',class-sym ,package)
               ;`(def-java-constructors ,full-class-name)
               ;`(def-java-methods ,full-class-name)
               ;`(def-java-fields ,full-class-name)
               (let ((supers (mapcar #'class-symbol bases)))
                 (append (mapcar (lambda (p)
                                   `(ensure-package ,(package-name p)))
                                 (remove (symbol-package class-sym)
                                         (remove-duplicates (mapcar #'symbol-package supers))))
                         (list `(defclass ,(class-symbol full-class-name)
                                          ,(or supers '(fref)) ()))
                         (do-def-members full-class-name canonic-class-sym package members))))))
        ;`(locally ,@defs)
        defs))))

(defun do-def-members (full-class-name class-sym package members)
  (append
   (do-def-ctors full-class-name class-sym package (find-tag :ctors members))
   (do-def-methods full-class-name class-sym package (find-tag :methods members))
   (do-def-fields full-class-name class-sym package (find-tag :fields members))
   (do-def-props full-class-name class-sym package (find-tag :properties members))))

(defgeneric make-new (class-sym &rest args)
  (:documentation "Allows for definition of before/after methods on ctors.
The new macro expands into call to this"))

(defmethod make-new ((full-class-name string) &rest args)
  (apply #'make-new (canonic-class-symbol full-class-name) args))

#|
  "class-spec -> class-sym | (class-sym this-name)
class-sym -> classname.
args -> as per ctors and make-new
init-arg-spec -> init-arg | (init-arg)

Creates a new instance of class, using make-new generic function,
then runs the body replacing all top-level calls of the form
(.anything whatever)
with
(classname.anything new-object whatever)
If this-name is supplied it will be bound to the newly-allocated object and available
to the body (note - but not to the args!)
"
|#

(defmacro new (class-spec args &body body)
  (let ((class-sym (if (symbolp class-spec)
                       class-spec
                     (first class-spec)))
        (this (if (listp class-spec)
                  (second class-spec)
                (gensym))))
    `(let ((,this (make-new ,class-sym ,@args)))
       (progn
         ,@(mapcar (lambda (form)
                     (if (and (listp form)
                              (symbolp (first form))
                              (eql 0 (position #\. (symbol-name (first form)))))
                         (list* (find-symbol (string-upcase (string-append
                                                             (symbol-name class-sym)
                                                             (subseq (symbol-name (first form)) 1)))
                                             (symbol-package class-sym))
                                this
                                (rest form))
                       form))
                   body))
       ,this)))

(defun call-ctor (class-sym args)
  (let ((class (find-class-ref class-sym))
        (inits (member-if #'keywordp args)))
    (if inits
        (apply #'send-message :new class *marshalling-flags* *marshalling-depth*
               (ldiff args inits) inits)
      (send-message :new class *marshalling-flags* *marshalling-depth* args))))

(defun do-def-ctors (full-class-name class-sym package ctor-list)
"creates and exports a ctor func classname.new, defines a method of 
make-new specialized on the class-symbol"
  (when ctor-list
    (let ((ctor-sym (constructor-symbol full-class-name)))
      `((defun ,ctor-sym (&rest args)
          ,(format nil "~{~A~%~}" ctor-list)
          (call-ctor ',class-sym args))
        (export ',ctor-sym ,package)
        (defmethod make-new ((class-sym (eql ',class-sym)) &rest args)
          (apply (function ,ctor-sym) args))))))

(defmacro with-vm (vm &body body)
  `(let ((*fvm* ,vm))
     ,@body))

(defmacro with-vm-of (this &body body)
  (let ((gthis (gensym)))
  `(let* ((,gthis ,this)
          (*fvm* (if (and ,gthis (typep ,gthis 'fref))
                     (fref-vm ,gthis)
                   *fvm*)))
     ,@body)))

(defmacro with-marshalling ((depth &rest flags) &body body)
  `(let ((*marshalling-depth* ,depth)
         (*marshalling-flags* (logior ,@flags)))
     ,@body))

(defun foil-call-method (class-sym name method-sym this args)
  (with-vm-of this
    (let ((cref (find-method-ref class-sym name method-sym)))
      (apply #'send-message :call cref *marshalling-flags* *marshalling-depth* this args))))

(defun do-def-methods (full-class-name class-sym package methods)
  (let ((methods-by-name (make-hash-table :test #'equal))
        (defs nil))
    (dolist (method methods)
      (push method (gethash (find-tag-atom :name method) methods-by-name)))
    (maphash
     (lambda (name methods)
       (let ((method-sym (member-symbol full-class-name name))
             (is-static (find-tag-atom :static (first methods)))
             (doc (format nil "~{~A~%~}" (mapcar (lambda (m)
                                                   (find-tag-atom :doc m))
                                                 methods))))
         (push (if is-static
                   `(defun ,method-sym (&rest args)
                      ,doc
                      (foil-call-method ',class-sym ,name ',method-sym nil args))
                 `(defun ,method-sym (this &rest args)
                    ,doc
                    (foil-call-method ',class-sym ,name ',method-sym this args)))
               defs)
         (push `(export ',method-sym ,package)
               defs)))
     methods-by-name)
    (nreverse defs)))



#|
all public fields will get a getter function classname.fieldname and a setter - (setf classname.fieldname)
instance fields take an first arg which is the instance
static fields also get a symbol-macro *classname.fieldname*
|#

(defun call-field (class-sym name field-sym this &rest args)
  (with-vm-of this
    (let ((cref (find-field-ref class-sym name field-sym)))
      (apply #'send-message :call cref *marshalling-flags* *marshalling-depth* this args))))

(defun do-def-fields (full-class-name class-sym package fields)
  "fields will get a getter function classname.fieldname and a setter - (setf classname.fieldname)
instance fields take an first arg which is the instance
static fields also get a symbol-macro *classname.fieldname*"
  (let* ((defs nil))
    (dolist (field fields)
      (let* ((field-name (find-tag-atom :name field))
             (field-sym (member-symbol full-class-name field-name))
             (is-static (find-tag-atom :static field))
             (doc (find-tag-atom :doc field)))
        (if is-static
            (let ((macsym (intern (string-append "*" (symbol-name field-sym) "*")
                                  package)))
              (push `(defun ,field-sym ()
                       ,doc
                       (call-field ',class-sym ,field-name ',field-sym nil))
                    defs)
              (push `(defun (setf ,field-sym) (val)
                       (call-field ',class-sym ,field-name ',field-sym nil val))
                    defs)
              (push `(export ',field-sym ,package) defs)
              (push `(define-symbol-macro ,macsym (,field-sym)) defs)
              (push `(export ',macsym ,package) defs))
          (progn
            (push `(defun ,field-sym (obj)
                     ,doc
                     (call-field ',class-sym ,field-name ',field-sym obj))
                  defs)
            (push `(defun (setf ,field-sym) (val obj)
                     (call-field ',class-sym ,field-name ',field-sym obj val))
                  defs)
            (push `(export ',field-sym ,package) defs)))))
    (nreverse defs)))

(defun call-prop-get (class-sym name method-sym this args)
  (with-vm-of this
;    (format t "prop-get ~A meth ~A~%" name method-sym)
    (let ((cref (find-prop-get class-sym name method-sym)))
      (apply #'send-message :call cref *marshalling-flags* *marshalling-depth* this args))))

(defun call-prop-set (class-sym name method-sym this args)
  (with-vm-of this
;    (format t "prop-set ~A meth ~A~%" name method-sym)
    (let ((cref (find-prop-set class-sym name method-sym)))
      (apply #'send-message :call cref *marshalling-flags* *marshalling-depth* this args))))

(defun do-def-props (full-class-name class-sym package props)
  (let ((props-by-name (make-hash-table :test #'equal))
        (defs nil))
    (dolist (prop props)
      (push prop (gethash (find-tag-atom :name prop) props-by-name)))
    (maphash
     (lambda (name props)
       (let ((method-sym (member-symbol full-class-name (if (equals "Item" name) "iref" name)))
             (getter-sym (when (some (lambda (prop) (find-tag-atom :get-doc prop)) props)
                           (member-symbol full-class-name (string-append name "-get"))))
             (setter-sym (when (some (lambda (prop) (find-tag-atom :set-doc prop)) props)
                           (member-symbol full-class-name (string-append name "-set"))))
             (is-static (find-tag-atom :static (first props)))
             (get-doc (format nil "~{~A~%~}" (mapcar (lambda (p)
                                                   (find-tag-atom :get-doc p))
                                                 props)))
             (set-doc (format nil "~{~A~%~}" (mapcar (lambda (p)
                                                   (find-tag-atom :set-doc p))
                                                 props))))
         (when getter-sym
             (push (if is-static
                   `(defun ,method-sym (&rest args)
                      ,get-doc
                      (call-prop-get ',class-sym ,name ',getter-sym nil args))
                 `(defun ,method-sym (this &rest args)
                    ,get-doc
                    (call-prop-get ',class-sym ,name ',getter-sym this args)))
               defs))
         (when setter-sym
             (push (if is-static
                   `(defun (setf ,method-sym) (val &rest args)
                      ,set-doc
                      (call-prop-set ',class-sym ,name ',setter-sym nil (append args (list val))))
                     `(defun (setf ,method-sym) (val this &rest args)
                        ,set-doc
                        (call-prop-set ',class-sym ,name ',setter-sym this (append args (list val)))))
                   defs))
         (push `(export ',method-sym ,package)
               defs)))
     props-by-name)
    (nreverse defs)))

(defmacro def-foil-class (full-class-name)
  "Given the package-qualified, case-correct name of a java class, will generate
wrapper functions for its constructors, fields and methods."
  `(locally ,@(do-def-foil-class full-class-name)))

(defun get-library-classnames (jar-or-assemby &rest packages)
  (apply #'send-message :cnames jar-or-assemby packages))

(defun dump-wrapper-defs-to-file (filename classnames)
  "given a list of classnames (e.g. from get-jar-classnames or get-assembly-classnames), writes
the expansions of def-foil-class to a file. 
The resulting file will not need a VM running to either compile or load"
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (dump-wrapper-defs s classnames))
  filename)

(defun dump-wrapper-defs (strm classnames)
  (let ((forms nil)
        (ensures nil)
        (exports (make-hash-table :test #'equal)))
    (dolist (name (sort classnames #'string-lessp))
      (format t "def-foil ~A~%" name)
      (let ((defs (do-def-foil-class name)))
        (dolist (def defs)
          (case (car def) 
            ('ensure-package (push def ensures))
            ('export (push (second def) (gethash (third def) exports)))
            (t (push def forms))))))
    (format strm "(eval-when (:compile-toplevel :load-toplevel)~{~S~%~})~{~S~%~}"
            (remove-duplicates ensures :test #'string-equal :key #'second)
            (nreverse forms))
    (maphash (lambda (package syms)
               (format strm "(eval-when (:load-toplevel)(export '~S ~S))~%"
                       (mapcar #'second syms) package))
             exports)))

;;;;;;;;;;;;;;;;;;;;boxing ;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (fbox (:print-object print-fbox))
  type
  val)

(defun print-fbox (box strm)
  (format strm "#{:box ~S ~S}" (fbox-type box) (fbox-val box)))

(defun box (type val)
  (make-fbox :type (type-arg type) :val val))

(defstruct (fvbox (:print-object print-fvbox))
  type
  vals)

(defun print-fvbox (box strm)
  (format strm "#{:vector ~S~{ ~S~}}" (fvbox-type box) (fvbox-vals box)))

(defun box-vector (type &rest vals)
  (make-fvbox :type (type-arg type) :vals vals))

;;;;;;;;;;;;;;;;vectors;;;;;;;;;;;;;;;;

(defun make-new-vector (type length &rest inits)
  (apply #'send-message :vector (type-arg type) length inits))

(defun vref (vec idx)
  (send-message :vget vec *marshalling-flags* *marshalling-depth* idx))

(defun (setf vref) (val vec idx)
  (send-message :vset vec idx val)
  val)

(defun vlength (vec)
  (send-message :vlen vec))

;;;;;;;;;;;;;portable object stuff;;;;;;;;;;;;;

(defun equals (fref1 fref2)
  (or (eql fref1 fref2)
      (and fref1 fref2 (send-message :equals fref1 fref2))))

(defun instance-of (fref type)
  (send-message :is-a fref (type-arg type)))

(defun to-string (fref)
  (send-message :str fref))

(defun hash (fref &key rehash)
  (if rehash
      (setf (fref-hash fref) (send-message :hash fref))
    (get-or-init (fref-hash fref)
                 (send-message :hash fref))))

(defun marshall (fref)
  (send-message :marshall fref *marshalling-flags* *marshalling-depth*))

;;;;;;;;;;;;;;;;;;; proxies ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-new-proxy (arg-marshall-flags arg-marshall-depth &rest interfaces)
  (apply #'send-message :proxy arg-marshall-flags arg-marshall-depth (mapcar #'type-arg interfaces)))

(defgeneric handle-proxy-call (method-symbol proxy &rest args))

(defmethod handle-proxy-call (method-symbol proxy &rest args)
  (format t "unhandled :proxy-call ~S ~S ~S~%" method-symbol proxy args)
  nil)


(defmacro new-proxy (proxy arg-marshall-flags arg-marshall-depth &rest interface-defs)
  "proxy -> a symbol
interface-def -> (interface-name method-defs+)
interface-name -> classname. (must name an interface type)
method-def -> (method-name (args*) body)
method-name -> symbol (without classname)

Creates, and returns an object that implements the supplied interfaces. The symbol proxy
will be bound to the proxy instance in the body of the method implementations.
arg-marshall-flags and arg-marshall-depth will be used to marshall the arguments to the proxy
methods when they are subsequently invoked"
  (let ((method-sym (gensym))
        (args-sym (gensym)))
    `(let ((,proxy (make-new-proxy ,arg-marshall-flags ,arg-marshall-depth
                                   ,@(mapcar #'car interface-defs))))
       ,@(mapcan
          (lambda (interface-def)
            (mapcar
             (lambda (method-def)
               (destructuring-bind (method-name args &body body) method-def
                 `(defmethod handle-proxy-call
                             ((,method-sym (eql ',(find-symbol (string-upcase
                                                                (string-append (first interface-def)
                                                                               method-name))
                                                               (symbol-package (first interface-def)))))
                              (,proxy (eql ,proxy))
                              &rest ,args-sym)
                    (destructuring-bind ,args ,args-sym
                      ,@ body))))
             (rest interface-def)))
          interface-defs)
       ,proxy)))

;;; Eric
;;;;;;;;;;;;;;;;indexer-support;;;;;;;;;;;;;;;;
(defun iref (indexable-obj &rest inxs)
  (apply #'send-message :iget indexable-obj *marshalling-flags* *marshalling-depth* inxs))

(defun (setf iref) (val obj-ref &rest inxs)
  (apply #'send-message :iset obj-ref (append inxs (list val))))

