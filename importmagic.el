;;; importmagic.el --- A package that autoimports using importmagic.
;;; Commentary:
;;; Code:

(require 'epc)

(defvar importmagic-server (epc:start-epc "python" '("tester.py"))
  "A variable that holds the importmagic.el EPC server.")


(epc:stop-epc importmagic-server)
(setq importmagic-server (epc:start-epc "python" '("tester.py")))
(epc:call-sync importmagic-server
               'get_candidates_for_symbol
               `(,buffer-file-name ))

(defun importmagic--fix-imports (import-block start end)
  "Insert given IMPORT-BLOCK with import fixups in the current
buffer starting in line START and ending in line END."
  (save-restriction
    (save-excursion
      (widen)
      (goto-char (point-min))
      (forward-line start)
      (let ((start-pos (point))
            (end-pos (progn (forward-line (- end start)) (point))))
        (delete-region start-pos end-pos)
        (insert import-block)))))

(defun importmagic--query-imports-for-statement (statement)
  "Query importmagic server for STATEMENT imports in the current buffer."
  (let* ((specs (epc:call-sync importmagic-server
                               'get_import_statement
                               `(,buffer-file-name ,statement)))
         (start (car specs))
         (end (cadr specs))
         (theblock (caddr specs)))
    (importmagic--fix-imports theblock start end)))

(defun importmagic-fix-symbol-at-point ()
  "Query the RPC server for a suitable candidate to add to
imports in order to correctly import the symbol at point. The
default candidate is the most suitable. The selected candidate is
then added to the import list at the top of the file."
  (interactive)
  (let* ((thesymbol (thing-at-point 'symbol t))
         (options (epc:call-sync importmagic-server
                                 'get_candidates_for_symbol
                                 thesymbol)))
    (if (not options)
        (error "No suitable candidates found for %s" thesymbol)
      (let ((choice (completing-read (concat "Querying for " thesymbol ": ")
                                     options
                                     nil
                                     t
                                     nil
                                     nil
                                     options)))
        (message (importmagic--query-imports-for-statement choice))))))




(provide 'importmagic)
;;; importmagic.el ends here
