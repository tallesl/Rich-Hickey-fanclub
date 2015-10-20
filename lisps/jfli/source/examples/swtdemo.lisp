;Just load this from LispWorks menu
;note works on Windows, some issues on OS X due to windowing conflict w/IDE
(load "~/jfli/jni")
(load "~/jfli/jfli")
(use-package :jfli)
(create-jvm 
#+:macosx "-Djava.class.path=/Users/rich/swt/swt.jar:/Users/rich/swt/swt-pi.jar:User/rich/jfli/jfli.jar"
 #+:macosx "-Djava.library.path=/Users/rich/swt"
 #+:win32 "-Djava.class.path=/swt/swt.jar;/lisp/jfli.jar"
 #+:win32 "-Djava.library.path=/swt"
 )
(enable-java-proxies)



(def-java-class "org.eclipse.swt.widgets.Display")
(def-java-class "org.eclipse.swt.widgets.Button")
(def-java-class "org.eclipse.swt.widgets.Shell")
(def-java-class "org.eclipse.swt.widgets.Listener")
(def-java-class "org.eclipse.swt.SWT")

(use-package "org.eclipse.swt")
(use-package "org.eclipse.swt.widgets")


(defun swt-demo ()
  (let* ((display (new display.))
         (shell (new shell. display
                     :gettext "Using SWT from Lisp"
                     (.setsize 300 200)
                     (.setlocation 100 100)))
         (button (new (button. this) shell *SWT.CENTER*
                      :gettext "Call Lisp"
                      (.addlistener *swt.selection*
                       (new-proxy (listener.
                                   (handleevent (event)
                                     (declare (ignore event))
                                     (setf (button.gettext this)
                                           (format nil "~A ~A"
                                                   (lisp-implementation-type)
                                                   (lisp-implementation-version)))
                                     nil))))
                      (.setsize 200 100)
                      (.setlocation 40 40))))
    (declare (ignore button))
    (shell.open shell)
    (do ()
        ((shell.isdisposed shell))
      (unless (display.readanddispatch display)
        (display.sleep display)))
    (display.dispose display)))

(mp:process-run-function "swt-proc" '() #'swt-demo)

