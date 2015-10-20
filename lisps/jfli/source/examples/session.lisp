;this presumes you have already compiled jni.lisp and jfli.lisp into fasl files

CL-USER 4 > (load "/lisp/jni")
; Loading fasl file C:\lisp\jni.fsl
#P"C:/lisp/jni.fsl"

CL-USER 5 > (load "/lisp/jfli")
; Loading fasl file C:\lisp\jfli.fsl
#P"C:/lisp/jfli.fsl"

;The user API is entirely in the jfli package
CL-USER 6 > (use-package :jfli)
T

;tell the library where Java is located
CL-USER 7 > (setf *jni-lib-path* "/j2sdk1.4.2_01/jre/bin/client/jvm.dll")
"/j2sdk1.4.2_01/jre/bin/client/jvm.dll"

;this starts the VM - note how you can set the classpath
CL-USER 8 > (create-jvm "-Djava.class.path=/lisp/jfli.jar")
0
#<Pointer: JNI:PVM = #x081022A0>
#<Pointer: JNI:PENV = #x0086A858>

;define wrappers for the members of Object
CL-USER 9 > (def-java-class "java.lang.Object")
NIL

;and of Properties, a Hashtable-like class
CL-USER 10 > (def-java-class "java.util.Properties")
#<STANDARD-CLASS |java.util|:PROPERTIES. 2066B964>

;the above will create these packages if they do not already exist
;use the packages for easy name access

CL-USER 11 > (use-package "java.lang")
T

CL-USER 12 > (use-package "java.util")
T

;create a Properties instance, note keyword-style member inits, string conversion etc
;also note typed return value
CL-USER 13 > (setf p (new properties. :getproperty "fred" "ethel"))
#<PROPERTIES. 20664A94>

;virtual functions work as normal
CL-USER 14 > (object.tostring p)
"{fred=ethel}"

;setter was generated for member function because it follows the JavaBeans property protocol
CL-USER 15 > (setf (properties.getproperty p "ricky") "lucy")
"lucy"

CL-USER 16 > (object.tostring p)
"{ricky=lucy, fred=ethel}"

CL-USER 17 > (properties.size p)
2

;totally dynamic access, create wrappers as you need
CL-USER 18 > (def-java-class "java.lang.Class")
#<STANDARD-CLASS CLASS. 20680EC4>

CL-USER 19 > (class.getname (object.getclass p))
"java.util.Properties"

CL-USER 20 > (def-java-class "java.util.Enumeration")
#<STANDARD-CLASS ENUMERATION. 20669274>

;no need to wait for the vendor to enhance the language - you use Lisp!
CL-USER 21 > (defmacro doenum ((e enum) &body body)
  (let ((genum (gensym)))
    `(let ((,genum ,enum))
       (do ()
           ((not (enumeration.hasmoreelements ,genum)))
         (let ((,e (enumeration.nextelement ,genum)))
           ,@body)))))(defmacro doenum ((e enum) &body body)
               (let ((genum (gensym)))
                 `(let ((,genum ,enum))
                    (do ()
                        ((not (enumeration.hasmoreelements ,genum)))
                      (let ((,e (enumeration.nextelement ,genum)))
                        ,@body)))))
DOENUM

;can't do this in Java yet can in Lisp
CL-USER 22 > (doenum (prop (properties.elements p)) (print (object.tostring prop)))

"lucy" 
"ethel" 
NIL

;doc strings are created giving original Java signatures and indicating overloads
CL-USER 23 > (documentation 'properties.getproperty 'function)
"java.lang.String getProperty(java.lang.String,java.lang.String)
java.lang.String getProperty(java.lang.String)
"

CL-USER 24 > (documentation 'properties.new 'function)
"java.util.Properties()
java.util.Properties(java.util.Properties)
"

