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

;;; importmagic-tests.el ends here
