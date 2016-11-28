;;; importmagic.el --- A package that autoimports using importmagic.
;;; Commentary:
;;; Code:

(require 'epc)
(require 'f)

;; This process of creating a mode is rather painless. Thank you
;; nullprogram!
;; http://nullprogram.com/blog/2013/02/06/

(defvar importmagic-auto-update-index t
  "Set to nil if you don't want to auto-update importmagic's symbol index after saving.")
(defvar importmagic-server nil
  "The importmagic index server.")
(make-variable-buffer-local 'importmagic-server)

;;;###autoload
(define-minor-mode importmagic-mode
  "A mode that lets you autoimport unresolved Python symbols."
  :init-value nil
  :lighter " import"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-c i") 'importmagic-fix-symbol-at-point)
            keymap)
  (when (not (derived-mode-p 'python-mode))
    (error "Importmagic only works with Python buffers"))
  (let ((importmagic-path (f-slash (f-dirname (locate-library "importmagic")))))
    (if importmagic-mode
        (progn
          (setq importmagic-server
                (epc:start-epc "python"
                               `(,(f-join importmagic-path "importmagicserver.py"))))
          (add-hook 'kill-buffer-hook 'importmagic--teardown-epc)
          (importmagic--async-add-dir (f-dirname (f-this-file))))
      (epc:stop-epc importmagic-server)
      (setq importmagic-server nil))))

(defun importmagic--teardown-epc ()
  "Stop the EPC server for the current buffer."
  (when (and (derived-mode-p 'python-mode)
             importmagic-server
             (symbolp 'importmagic-mode)
             (symbol-value 'importmagic-mode))
    (epc:stop-epc importmagic-server)))

(defun importmagic--buffer-as-string ()
  "Return the whole contents of the buffer as a single string."
  (buffer-substring-no-properties (point-min) (point-max)))

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


(defun importmagic--query-imports-for-statement-and-fix (statement)
  "Query importmagic server for STATEMENT imports in the current buffer."
  (let* ((specs (epc:call-sync importmagic-server
                               'get_import_statement
                               `(,(importmagic--buffer-as-string) ,statement)))
         (start (car specs))
         (end (cadr specs))
         (theblock (caddr specs)))
    (importmagic--fix-imports theblock start end)))

(defun importmagic-fix-symbol (symbol)
  "Fix imports for SYMBOL in current buffer."
  (interactive "sSymbol: ")
  (let ((options (epc:call-sync importmagic-server
                                'get_candidates_for_symbol
                                symbol)))
    (if (not options)
        (error "[importmagic] No suitable candidates found for %s" symbol)
      (let ((choice (completing-read (concat "Querying for " symbol ": ")
                                     options
                                     nil
                                     t
                                     nil
                                     nil
                                     options)))
        (importmagic--query-imports-for-statement-and-fix choice)
        (message "[importmagic] Inserted %s" choice)))))

(defun importmagic-fix-symbol-at-point ()
  "Fix imports for symbol at point."
  (interactive)
  (importmagic-fix-symbol (thing-at-point 'symbol t)))

(defun importmagic--get-unresolved-symbols ()
  "Query the RPC server for every unresolved symbol in the current file."
  (epc:call-sync importmagic-server 'get_unresolved_symbols (importmagic--buffer-as-string)))

(defun importmagic-fix-imports ()
  "Fix every possible import in the file."
  (interactive)
  (let ((unresolved (importmagic--get-unresolved-symbols))
        (no-candidates '()))
    (dolist (symbol unresolved)
      (condition-case nil
          (importmagic-fix-symbol symbol)
        (error (setq no-candidates (push symbol no-candidates)))))
    (when no-candidates
      (message "[importmagic] Symbols with no candidates: %s" no-candidates))))

(defun importmagic--auto-update-index ()
  "Update importmagic symbol index with current directory."
  (when (derived-mode-p 'python-mode)
    (importmagic--async-add-dir
     (f-dirname (f-this-file)))))

(defun importmagic--async-add-dir (path)
  "Asynchronously add PATH to index symbol."
  (deferred:$
    (epc:call-deferred importmagic-server 'add_directory_to_index path)
    (deferred:nextc it
      `(lambda (result)
         (if (stringp result)
             (error "[importmagic] Couldn't update index")
           (message "[importmagic] Indexed %s" ,path))))))



(provide 'importmagic)
;;; importmagic.el ends here
