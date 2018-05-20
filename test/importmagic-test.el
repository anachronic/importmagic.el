;;; importmagic-tests.el --- Tests for importmagic.el.
;;; Commentary:
;;; Code:


(require 'ert)
(require 's)
(require 'importmagic)


;; Server available and importmagic can run on Python-mode buffers
(ert-deftest importmagic-server-available ()
  (with-temp-buffer
    (importmagic--testcase
     (should importmagic-server))))

;; With python as interpreter
(ert-deftest importmagic-use-vanilla-python ()
  (with-temp-buffer
    (setq-default importmagic-python-interpreter "python")
    (python-mode)
    (importmagic-mode)
    (should importmagic-server)))

;; ;; With ipython as interpreter
;; (ert-deftest importmagic-use-vanilla-ipython ()
;;   (with-temp-buffer
;;     (setq-default importmagic-python-interpreter "ipython")
;;     (python-mode)
;;     (importmagic-mode)
;;     (should importmagic-server)))

;; ;; With ipython with one argument
;; (ert-deftest importmagic-use-ipython-with-one-arg ()
;;   (with-temp-buffer
;;     (setq-default importmagic-python-interpreter "ipython --no-banner")
;;     (python-mode)
;;     (importmagic-mode)
;;     (should importmagic-server)))

;; ;; With ipython with two arguments
;; (ert-deftest importmagic-use-ipython-with-two-args ()
;;   (with-temp-buffer
;;     (setq-default importmagic-python-interpreter "ipython --no-banner --quick")
;;     (python-mode)
;;     (importmagic-mode)
;;     (should importmagic-server)))

;; ;; With ipython with three arguments
;; (ert-deftest importmagic-use-ipython-with-three-args ()
;;   (with-temp-buffer
;;     (setq-default importmagic-python-interpreter "ipython --no-banner --quick --quiet")
;;     (python-mode)
;;     (importmagic-mode)
;;     (should importmagic-server)))

;; Let's define a bad buffer
(defconst importmagic-bad-buffer-test
  "config_path = os.path.join(os.getcwd(), 'index.json')
today = datetime.now()
future = datetime.timedelta(hours=1)
")

;; After importing everything, the good buffer should look like the
;; following const
(defconst test/importmagic-bad-buffer-needs '("import os.path" "import os" "import datetime"))

;; Test that unresolved symbols return ok.
(ert-deftest importmagic-unresolved-symbols ()
  (with-temp-buffer
    (insert importmagic-bad-buffer-test)
    (importmagic--testcase
     (let ((expected-symbols '("os.path.join" "os.getcwd" "datetime.timedelta" "datetime.now"))
           (actual-symbols (importmagic--get-unresolved-symbols)))
       (dolist (symbol expected-symbols)
         (should (member symbol actual-symbols)))))))

;; Every test from now on rebind the completing read function with
;; cl-letf. I really struggled with it. So thanks @Malabarba
;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html

;; Test that importmagic-fix-imports ends up with a good buffer
(ert-deftest importmagic-fix-imports-good ()
  (with-temp-buffer
    (importmagic--testcase
     (progn
       (insert importmagic-bad-buffer-test)
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (prompt vals pred match initial history default)
                    (cond
                     ((string= "Querying for os.path.join: " prompt) "import os.path")
                     ((string= "Querying for os.getcwd: " prompt) "import os")
                     ((string= "Querying for datetime.timedelta: " prompt) "import datetime")
                     ((string= "Querying for datetime.now: " prompt) "import datetime")
                     (t nil)          ; Fail otherwise
                     ))))
         (importmagic-fix-imports)
         (dolist (required-symbol test/importmagic-bad-buffer-needs)
           (should (s-contains? required-symbol (importmagic--buffer-as-string)))))))))

;; Test that a dummy buffer (with "import os") fixes the unresolved
;; symbol "os" by manually typing it.
(ert-deftest importmagic-query-symbol-good ()
  (with-temp-buffer
    (importmagic--testcase
     (insert "os.path")
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &optional predicate require-match initial history default inherit-input-method)
                  "import os")))
       (importmagic-fix-symbol "os")
       (should (s-contains? "import os" (importmagic--buffer-as-string)))))))

;; Test symbol at point is ok.
(ert-deftest importmagic-fix-at-point-good-2 ()
  (with-temp-buffer
    (importmagic--testcase
     (insert "os.path")
     (backward-char 6)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &optional predicate require-match initial history default inherit-input-method)
                  "import os")))
       (importmagic-fix-symbol-at-point)
       (should (s-contains? "import os" (importmagic--buffer-as-string)))))))


;; Should fail on other major modes. Maybe add more for completeness?
(ert-deftest importmagic-only-python ()
  (with-temp-buffer
    (fundamental-mode)
    (should-error (importmagic-mode)))
  (with-temp-buffer
    (c-mode)
    (should-error (importmagic-mode))))

;; Test that EPC variable is initially nil for a buffer, even a Python
;; buffer. This will fail using M-x ert if you have a hook on python
;; mode to activate importmagic-mode
(ert-deftest importmagic-server-unavailable ()
  (with-temp-buffer
    (fundamental-mode)
    (should (not importmagic-server)))
  (with-temp-buffer
    (python-mode)
    (should (not importmagic-server))))

;; Self-explanatory: Server should be stopped and references should be
;; destroyed upon importmagic's deactivation
(ert-deftest importmagic-epc-server-is-actually-destroyed ()
  (with-temp-buffer
    (importmagic--testcase
     (should importmagic-server)
     (importmagic-mode -1)
     (should (not importmagic-server)))))

;; Other buffers shouldn't inherit current buffer's EPC server
(ert-deftest importmagic-epc-server-is-actually-buffer-local ()
  (let ((buffer1 (get-buffer-create "*A Python Buffer*"))
        (buffer2 (get-buffer-create "*Another Python buffer*")))
    (unwind-protect
        (with-temp-buffer
          buffer1
          (importmagic--testcase
           (should importmagic-server))
          (with-temp-buffer
            buffer2
            (python-mode)
            (should (not importmagic-server))
            (condition-case nil
                (progn
                  (importmagic-mode)
                  (should importmagic-server))
              (error (progn
                       (should (not importmagic-server))
                       (should (not (symbol-value 'importmagic-mode)))
                       (should (not (epc:live-p importmagic-server)))))))))))




;;; importmagic-tests.el ends here
