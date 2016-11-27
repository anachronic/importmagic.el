;;; importmagic.el --- A package that autoimports using importmagic.
;;; Commentary:
;;; Code:

(require 'epc)
(require 'f)

;; This process of creating a mode is rather painless. Thank you
;; nullprogram!
;; http://nullprogram.com/blog/2013/02/06/

;;;###autoload
(define-minor-mode importmagic-mode
  :init-value nil
  :lighter " import"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-c i") 'importmagic-fix-symbol-at-point)
            keymap)
  (when (not (derived-mode-p 'python-mode))
    (error "Importmagic only works with Python buffers"))
  (if importmagic-mode
      (make-variable-buffer-local
       (defvar importmagic-server
         (epc:start-epc "python"
                        `(,(f-join (f-dirname (locate-library "importmagic"))
                                   "importmagicserver.py")))
         "The importmagic server for the current buffer. It is local."))
    (when (boundp 'importmagic-server)
      (epc:stop-epc importmagic-server))))

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
        (error "No suitable candidates found for %s" symbol)
      (let ((choice (completing-read (concat "Querying for " symbol ": ")
                                     options
                                     nil
                                     t
                                     nil
                                     nil
                                     options)))
        (importmagic--query-imports-for-statement-and-fix choice)
        (message "Inserted %s" choice)))))

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
      (message "Symbols with no candidates: %s" no-candidates))))


(defun importmagic-update-index ()
  "Intelligently update symbol index depending on the current directory/file."
  (interactive)
  (let* ((thisfile (f-this-file))
         (thisdir (f-dirname thisfile))
         (package (f-join thisdir "__init__.py"))
         (path))
    (if (f-exists? package)
        (progn
          (importmagic--add-path-to-index thisdir)
          (setq path thisdir))
      (importmagic-add-path-to-index thisfile)
      (setq path thisfile))
    (message "Indexed %s for importmagic" path)))

(defun importmagic--add-path-to-index (path)
  "Add the specified PATH to the server's symbol index."
  (let ((return-val (epc:call-sync importmagic-server 'add_path_to_index path)))
    (when (stringp return-val)
      (error "Symbol index not ready, hold on please"))))



(provide 'importmagic)
;;; importmagic.el ends here
