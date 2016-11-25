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

(deferred:$
  (epc:call-deferred epc3 'get_candidates_for_symbol "os.path.join")
  (deferred:nextc it
    (lambda (x) (let ((res x))
             (while res
               (print (car res))
               (setq res (cdr res)))))))

(deferred:$
  (epc:call-deferred epc3 'get_import_statement `(,(expand-file-name "bad_buffer.py") "render"))
  (deferred:nextc it
    (lambda (x) (message "returned %s" x))))

(epc:stop-epc epc3)


(provide 'importmagic)
;;; importmagic.el ends here
