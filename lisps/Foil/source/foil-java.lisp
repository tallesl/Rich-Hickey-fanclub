(use-package :foil)

;this is just an example of some wrapper gen invocations, do similar stuff for your system
(defun gen-java-wrappers ()
  (compile-file
   (dump-wrapper-defs-to-file "/foil/java-lang.lisp"
                              (get-library-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/lang/")))
  (compile-file
   (dump-wrapper-defs-to-file "/foil/java-io.lisp"
                              (get-library-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/io/")))
  (compile-file
   (dump-wrapper-defs-to-file "/foil/java-util.lisp"
                              (get-library-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/util/")))

  (compile-file
   (dump-wrapper-defs-to-file "/foil/java-sql.lisp"
                              (get-library-classnames "/j2sdk1.4.2/jre/lib/rt.jar" "java/sql/")))

  ;sometimes LW 4.4 dies after successfully generating this fasl - hmmm...
  (compile-file
   (dump-wrapper-defs-to-file "/foil/swt.lisp"
     (get-library-classnames "/eclipse/plugins/org.eclipse.swt.win32_3.0.1/ws/win32/swt.jar"
                         "org/eclipse/swt/"
                         "org/eclipse/swt/program/" "org/eclipse/swt/printing/"
                         "org/eclipse/swt/layout/" "org/eclipse/swt/dnd/"
                         "org/eclipse/swt/graphics/" "org/eclipse/swt/custom/"
                         "org/eclipse/swt/events/" "org/eclipse/swt/printing/"
                         "org/eclipse/swt/widgets/" "org/eclipse/swt/browser/"
                         "org/eclipse/swt/awt/")))
  
  (compile-file
   (dump-wrapper-defs-to-file "/foil/commons-httpclient.lisp"
     (get-library-classnames
      "/dev/commons-httpclient-2.0.2/commons-httpclient-2.0.2.jar" "org/apache/commons/httpclient"))))
