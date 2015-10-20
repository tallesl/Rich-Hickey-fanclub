;Copyright (c) 2003, Rich Hickey
;licensed under the BSD license - see license.txt

(load-assembly "System.Windows.Forms")

(def (clip-get &key (format DataFormats:Text)) 
	(let (d (Clipboard:GetDataObject)) 
		(when (and d (d.GetDataPresent format)) 
			(d.GetData format))))

(def (clip-set data &key (keep true)) 
	(Clipboard:SetDataObject data keep))

(def (clip-load)
	(with-dispose (r (StringReader. (clip-get)))
		(interpreter.Load r)))
		
(def-macro (while-> test &rest body)
	`(let (it ~test)
		(while it
			(block
				~@body
				(set it ~test)))))
					
(def (clip-eval)
	(with-dispose (r (StringReader. (clip-get)))
		(for (it (interpreter.Read r)) (not (interpreter.Eof it)) (set it (interpreter.Read r))
			(prn (eval it)))))

