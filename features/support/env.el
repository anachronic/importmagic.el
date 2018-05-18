(require 'f)

(defvar importmagic.el-support-path
  (f-dirname load-file-name))

(defvar importmagic.el-features-path
  (f-parent importmagic.el-support-path))

(defvar importmagic.el-root-path
  (f-parent importmagic.el-features-path))

(add-to-list 'load-path importmagic.el-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'importmagic)
  (require 'espuds)
  (require 'ert))

(Setup)

(Before
 (switch-to-buffer
  (get-buffer-create "*importmagic*"))
 (erase-buffer)
 (fundamental-mode)
 (cua-mode 0)
 (transient-mark-mode t)
 (deactivate-mark)
 )

(After)

(Teardown)
