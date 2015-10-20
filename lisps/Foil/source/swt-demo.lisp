(load "/dev/foil/foil")
(use-package :foil)
(load "/foil/java-lang")
(load "/foil/java-util")
(load "/foil/swt")
(require "comm")

(defpackage :swt-demo
 (:use :cl :foil "org.eclipse.swt" "org.eclipse.swt.widgets" "org.eclipse.swt.events"
  "org.eclipse.swt.layout" "org.eclipse.swt.custom")
 (:export
  :*display*
  :init-display
  :run-ui
  :swt-apropos
  ))
(in-package :swt-demo)

;presumes swt-aware java server running on 2 ports, first will be ui

(defvar *ui-stream*)
(defvar *non-ui-stream*)
(defvar *display* nil)

(setf *ui-stream* (comm:open-tcp-stream "localhost" 13578))
(setf *non-ui-stream* (comm:open-tcp-stream "localhost" 13579))
(setf *fvm* (make-instance 'foreign-vm :stream *non-ui-stream*))

(def-foil-class "com.richhickey.foil.SWTHelper")


(defun init-display ()
  (let ((*thread-fvm* *fvm*)
        (*thread-fvm-stream* *ui-stream*))
    (setf *display* (make-new display.))))

(defun run-ui (fn)
  (let ((mp:*process-initial-bindings*
           (append '((*display* . *display*)
                     (*standard-output* . *standard-output*)
                     (*fvm* . *fvm*)
                     (*thread-fvm-stream* . *ui-stream*)
                     (*thread-fvm* . *fvm*))
                     mp:*process-initial-bindings*)))
      (mp:process-run-function "ui-proc" '() fn )))

(defun swt-apropos ()
  (let* ((shell (new shell. (*display* :text "SWT Apropos" :layout (gridlayout.new 1 t ))
                  (.setsize 800 600)
                  (.setlocation 100 100)))
         (top-row (new composite. (shell *SWT.NONE*
                                         :layout (gridlayout.new 5 nil)
                                         :layoutdata (griddata.new *GRIDDATA.FILL_HORIZONTAL*))))
         (l1 (label.new top-row *SWT.LEFT*
                        :text "Package:"))
         (packages-combo (combo.new top-row (logior *SWT.DROP_DOWN* *SWT.READ_ONLY*)))
         (l2 (label.new top-row *SWT.LEFT* :text "String:"))
         (search-text (text.new top-row (logior *SWT.SINGLE* *SWT.BORDER* *SWT.LEFT*)
                                :layoutdata (griddata.new *GRIDDATA.FILL_HORIZONTAL*)))
         (go-button (button.new top-row *SWT.CENTER* :text "Search"))
         (bottom-row (sashform.new shell *SWT.HORIZONTAL*
                                   :layoutdata (griddata.new *GRIDDATA.FILL_BOTH*)))
         (symbol-pane (composite.new bottom-row *SWT.NONE*
                                     :layout (gridlayout.new 1 t)))
         (l3 (label.new symbol-pane *SWT.LEFT*
                        :text "Symbols:"))
         (symbol-tree (tree.new symbol-pane (logior *SWT.SINGLE* *SWT.BORDER*)
                                :layoutdata (griddata.new *GRIDDATA.FILL_BOTH*)))
         (info-pane (composite.new bottom-row *SWT.NONE*
                                   :layout (gridlayout.new 1 t)))
         (l4 (label.new info-pane *SWT.LEFT*
                        :text "Doc:"))
         (doc-text (text.new info-pane (logior *SWT.MULTI* *SWT.BORDER* *SWT.LEFT*)
                              :layoutdata (griddata.new *GRIDDATA.FILL_BOTH*))))
    (declare (ignore l1 l2 l3 l4))
    ;init the display
    (dolist (p (cons "<all>" (sort  (mapcar #'package-name (list-all-packages)) #'string-lessp)))
      (combo.add packages-combo p))
    (combo.select packages-combo 0)
    (sashform.setweights bottom-row (box-vector :int 2 3))
    ;wire up events
    (labels ((gob ()
               (let* ((syms (remove-duplicates (apropos-list (text.text search-text)
                                                             (find-package (combo.text packages-combo)))))
                      (packages (remove-duplicates (mapcar #'symbol-package syms)))
                      (package-nodes (make-hash-table)))
                 (tree.setredraw symbol-tree nil)
                 (tree.removeall symbol-tree)
                 (dolist (p packages)
                   (setf (gethash p package-nodes)
                         (treeitem.new symbol-tree *SWT.NONE* :text (package-name p))))
                 (dolist (sym syms)
                   (treeitem.new (gethash (symbol-package sym) package-nodes) *SWT.NONE*
                                 :text (symbol-name sym)))
                 (tree.setredraw symbol-tree t))))
      (button.addselectionlistener go-button
        (new-proxy p +MARSHALL-ID+ 0
                   (selectionlistener.
                    (widgetselected (event)
                                    (declare (ignore event))
                                    (gob)
                                    nil))))
      (text.addkeylistener search-text
        (new-proxy p +MARSHALL-ID+ 0
                   (keylistener.
                    (keyreleased (event)
                                 (when (eql *SWT.CR*
                                            (keyevent.character event))
                                   (gob))))))
      (tree.addselectionlistener symbol-tree
        (new-proxy p +MARSHALL-ID+ 0
                   (selectionlistener.
                    (widgetselected (event)
                                    (let ((item (selectionevent.item event)))
                                      ;is it a leaf?
                                      (when (= 0 (treeitem.itemcount item))
                                        (let ((sym (find-symbol (treeitem.text item)
                                                                (treeitem.text (treeitem.parentitem item)))))
                                        (setf (text.text doc-text)
                                              (or
                                               (documentation sym 'function)
                                               (documentation sym 'variable)
                                               (documentation sym 'type)
                                               (documentation sym 'structure)
                                               "")))))
                                    nil))))
      ;launch
      (|com.richhickey.foil|::swthelper.rundispatchloop *display* shell))))

(init-display)
(run-ui #'swt-apropos)
