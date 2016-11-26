(require 'f)

(let ((impmagic-dir (f-dirname (f-dirname (f-this-file)))))
  (add-to-list 'load-path impmagic-dir))
