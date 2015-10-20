(use-package :foil)

;this is just an example of some wrapper gen invocations, do similar stuff for your system
(defun gen-cli-wrappers ()
  (compile-file
   (dump-wrapper-defs-to-file "/foil/cli-system-collections.lisp"
    (get-library-classnames
     "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System.Collections")))
  (compile-file
   (dump-wrapper-defs-to-file "/foil/cli-system.lisp"
    (get-library-classnames "/WINDOWS/Microsoft.NET/Framework/v1.1.4322/mscorlib.dll" "System/"))))
 