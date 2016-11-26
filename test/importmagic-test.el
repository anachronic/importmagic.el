;;; importmagic-tests.el --- Tests for importmagic.el.
;;; Commentary:
;;; Code:


(require 'ert)
(require 'importmagic)


;; Server available
(ert-deftest importmagic-server-available ()
  (should importmagic-server))


;; Let's define a bad buffer
(defconst importmagic-bad-buffer
  "config_path = os.path.join(os.getcwd(), 'index.json')
today = datetime.now()
future = datetime.timedelta(hours=1)
")

;; After importing everything, the good buffer should look like the
;; following const
(defconst importmagic-good-buffer
  "import datetime
import os

import os.path


config_path = os.path.join(os.getcwd(), 'index.json')
today = datetime.now()
future = datetime.timedelta(hours=1)
")

;; Test that unresolved symbols return ok.
(ert-deftest importmagic-unresolved-symbols ()
  (with-temp-buffer
    (insert importmagic-bad-buffer)
    (let ((expected-symbols '("os.path.join" "os.getcwd" "datetime.timedelta" "datetime.now"))
          (actual-symbols (importmagic--get-unresolved-symbols)))
      (dolist (symbol expected-symbols)
        (should (member symbol actual-symbols))))))

;; Test that importmagic-fix-imports ends up with a good buffer
(ert-deftest importmagic-fix-imports-good ()
  (with-temp-buffer
    (insert importmagic-bad-buffer)
    (let ((unread-command-events (listify-key-sequence (kbd "RET RET RET RET"))))
      (importmagic-fix-imports))
    (should (string= importmagic-good-buffer
                     (importmagic--buffer-as-string)))))

;; A dummy buffer with os.path should be able to import os
(defconst importmagic-dummy-good-buffer
  "import os


os.path")

;; Test a dummy buffer
(ert-deftest importmagic-query-symbol-good ()
  (with-temp-buffer
    (insert "os.path")
    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
      (importmagic-fix-symbol "os"))
    (should (string= importmagic-dummy-good-buffer
                     (importmagic--buffer-as-string)))))

;; Test that fix symbol at point is ok
(ert-deftest importmagic-fix-at-point-good ()
  (with-temp-buffer
    (insert "os.path")
    (backward-char 6)
    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
      (importmagic-fix-symbol-at-point))
    (should (string= importmagic-dummy-good-buffer
                     (importmagic--buffer-as-string)))))

;;; importmagic-tests.el ends here
