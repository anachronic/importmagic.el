;;; importmagic.el --- A package that autoimports using importmagic.
;;; Commentary:
;;; Code:

(require 'epc)

(defvar epc3 (epc:start-epc "python" '("tester.py")))
(setq epc3 (epc:start-epc "python" '("tester.py")))

(deferred:$
  (epc:call-deferred epc3 'get_unresolved_symbols (expand-file-name "bad_buffer.py"))
  (deferred:nextc it
    (lambda (x) (message "Return : %S" x))))

(epc:stop-epc epc3)


(provide 'importmagic)
;;; importmagic.el ends here
