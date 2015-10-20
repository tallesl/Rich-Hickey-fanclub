;;; Use the gen-cli-wrappers.lisp to generate the cli wrapper files. 
;;; The load-foil-and-cli-defsys.lisp has a function (load-foilcli-defsys) to compile and load the files. 
;;; You can also uncomment the block below to load the files.
;;#|
(defvar *dev-path* "/dev")
(load "/dev/foil/foil")
(use-package :foil)
(flet (( p-name (def-path)
         (format nil "~A/~A" (or *dev-path* (get-working-directory)) def-path)))
	(load (p-name "foil/foil-cli/cli-system"))
	(load (p-name "foil/foil-cli/cli-system-collections"))
	(load (p-name "foil/foil-cli/cli-reflection"))
	;(load (p-name "foil/foil-cli/cli-system-data-sqlclient"))
	(load (p-name "foil/foil-cli/cli-system-drawing"))
	(load (p-name "foil/foil-cli/cli-system-componentmodel"))
	(load (p-name "foil/foil-cli/cli-system-windows-forms-toplevel"))
	;(load (p-name "foil/foil-cli/foil-cli")
        )
;;|#

(eval-when (:compile-toplevel :load-toplevel)
(require "comm"))

; Assumes there is a cli server running 2 threads
(defvar *ui-stream*)
(defvar *non-ui-stream*)
(defvar *display*)

(setf *ui-stream* (comm:open-tcp-stream "localhost" 13479))
(setf *non-ui-stream* (comm:open-tcp-stream "localhost" 13478))
(setf *fvm* (make-instance 'foreign-vm :stream *non-ui-stream*))

(use-package "System")
(use-package "System.Collections")
(use-package "System.Windows.Forms")

(defvar *my-form* nil)

(defun run-demo ()
  (progn
    (init-display)
    (show-app2)))

(defun init-display ()
  (let ((*thread-fvm* *fvm*)
        (*thread-fvm-stream* *ui-stream*))))

; Helper for UI elements
(defmacro bld-ui-widget (ctor name loc size tabinx text)
               (destructuring-bind (n (lx ly) (sx sy) tab txt)
                   `(,name ,loc ,size ,tabinx ,text)
                   `(,ctor :Location (|System.Drawing|::Point.new ,lx ,ly)
                                  :Name ,n
                                  :Size (|System.Drawing|::Size.new ,sx ,sy)
                                  :TabIndex ,tab
                                  :Text ,txt)))
          
(defun show-app2()
  (let* ((symbolTreeView (TreeView.new :Anchor *Anchorstyles.Top* ; *Anchorstyles.Left* *Anchorstyles.Bottom* *Anchorstyles.Right*)  
                                       :ImageIndex -1
                                       :Location (|System.Drawing|::Point.new 8 72)
                                       :Name "symbolTreeView"
                                       :SelectedImageIndex -1
                                       :Size (|System.Drawing|::Size.new 256 576)
                                       :TabIndex 0))
         (packageLabel (bld-ui-widget Label.new "packageLabel" (8 8) (56 16) 0 "Package"))
         (stringLabel (bld-ui-widget Label.new "stringLabel" (304 8) (40 16) 1 "String"))
         (symbolLabel (bld-ui-widget Label.new  "symbolLabel" (8 48) (100 16) 2 "Symbols:"))
         (docLabel (bld-ui-widget Label.new "docLabel" (272 48) (100 16) 3 "Docs:"))
         (packageCombo (bld-ui-widget ComboBox.new "packageCombo" (80 8) (208 21) 2 ""))
         (searchTextBox (bld-ui-widget TextBox.new "searchTextBox" (360 8) (384 20) 3 ""))
         (searchButton (bld-ui-widget Button.new "searchButton" (760 8) (80 23) 4 "Search"))
         (searchPanel (Panel.new :BorderStyle *Borderstyle.Fixed3d* ;*Anchorstyles.Left* *Anchorstyles.Bottom* *Anchorstyles.Right*)  
                                 :Dock *Dockstyle.Top*
                                 :Location (|System.Drawing|::Point.new 0 0)
                                 :Name "searchPanel"
                                 :Text "searchPanel"
                                 :Size (|System.Drawing|::Size.new 856 40)
                                 :TabIndex 1))
         (docTextBox (TextBox.new :Anchor *Anchorstyles.Top* ;*Anchorstyles.Left* *Anchorstyles.Bottom* *Anchorstyles.Right*)  
                                  :Location (|System.Drawing|::Point.new 272 72)
                                  :Name "doctextBox"
                                  :Multiline t
                                  :Size (|System.Drawing|::Size.new 576 576)
                                  :TabIndex 4))
         (form (Form.new :AutoScaleBaseSize (|System.Drawing|::Size.new 5  13)
                         :ClientSize (|System.Drawing|::Size.new 856 654)
                         :Name "CLIForm"
                         :Text "CLI Apropos")))
    (Panel.Suspendlayout searchPanel)
    (Form.Suspendlayout form)
    (let ((ilist (Panel.Controls searchPanel))
          (flist (Form.Controls form)))     
      (mapcar (lambda (c)
                (IList.Add ilist c)) `(,searchButton ,searchTextBox ,packageCombo ,stringLabel ,packageLabel))
      (mapcar (lambda (c)
                (IList.Add flist c)) `(,docTextBox ,docLabel ,symbolLabel ,searchPanel ,symbolTreeView)))
    ;Init the data
    (let ((items (Combobox.Items packageCombo)))
      (dolist (p (cons "<all>" (sort  (mapcar #'package-name (list-all-packages)) #'string-lessp)))
        (ILIST.ADD items p)))
    (setf (Combobox.Selectedindex packageCombo) 0)
    (labels ((gob ()
               (let* ((syms (remove-duplicates (apropos-list (textbox.text searchTextBox)
                                                             (find-package (combobox.text packageCombo)))))
                      (packages (remove-duplicates (mapcar #'symbol-package syms)))
                      (package-nodes (make-hash-table))
                      (treenodes (treeview.nodes symboltreeview)))
                 (treeview.beginupdate symbolTreeView)
                 (treenodecollection.clear treenodes)
                 (dolist (p packages)
                   (setf (gethash p package-nodes)
                         (treenodecollection.add treenodes (package-name p))))
                 (dolist (sym syms)
                   (treenodecollection.add (treenode.nodes (gethash (symbol-package sym) package-nodes)) (symbol-name sym)))
                 (treeview.endupdate symbolTreeView)
                 (Panel.ResumeLayout searchPanel nil)
                 (Form.ResumeLayout form nil))))
      ;wire up the events
             (Button.add_click searchButton
               (new-proxy p1 +MARSHALL-ID+ 0
                          (eventhandler.
                           (invoke (sender event)
                                         (gob)))))
             (textbox.add_keyup searchtextbox
                                (new-proxy p2 +MARSHALL-ID+ 0
                                           (|System.Windows.Forms|::keyeventhandler.
                                            (invoke (sender event)
                                                    (when (eql 13 
                                                               (keyeventargs.keydata event))
                                                      (gob))))))
             (treeview.add_afterselect symboltreeview
               (new-proxy p3 +MARSHALL-ID+ 0
                          (|System.Windows.Forms|::treevieweventhandler.
                           (invoke (sender event)
                                   (let ((item (treevieweventargs.node event)))
                                      ;is it a leaf?
                                      (when (= 0 (treenodecollection.count (treenode.nodes item)))
                                        (let ((sym (find-symbol (treenode.text item)
                                                                (treenode.text (treenode.parent item)))))
                                          (setf (textbox.text docTextBox)
                                                (or
                                                 (documentation sym 'function)
                                                 (documentation sym 'variable)
                                                 (documentation sym 'type)
                                                 (documentation sym 'structure)
                                                 "")))))
                                   nil))))
             )
    ;Lanuch the UI thread
    (let ((mp:*process-initial-bindings*
           (append '((*standard-output* . *standard-output*)
                     (*fvm* . *fvm*)
                     (*thread-fvm-stream* . *thread-fvm-stream*)
                     (*thread-fvm* . *thread-fvm*))
                   mp:*process-initial-bindings*)))
      (mp:process-run-function
       "winform-proc" '()
       (lambda ()
         (|System.Windows.Forms|::Application.Run form))))))
      
