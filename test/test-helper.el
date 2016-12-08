(require 'f)

(let ((impmagic-dir (f-dirname (f-dirname (f-this-file)))))
  (add-to-list 'load-path impmagic-dir))


(defmacro importmagic--testcase (&rest body)
  "Run BODY in python and importmagic mode."
  (declare (indent 1))
  `(progn
     (python-mode)
     (importmagic-mode)
     (if (not importmagic-server)
         (progn
           (should (not importmagic-server)) ; duh
           (should (not (symbol-value 'importmagic-mode)))
           (should (not (epc:live-p importmagic-server))))
       ,@body)))
