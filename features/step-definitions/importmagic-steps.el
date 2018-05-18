;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^buffer is in \\(.+\\)$"
       "Turns on some mode."
       (lambda (mode)
         (let ((v (vconcat [?\C-u ?\M-x] (string-to-vector mode))))
           (execute-kbd-macro v))))

(Given "^the buffer has correctly started importmagic-mode$"
       (lambda ()
         (python-mode)
         (importmagic-mode)
         (sleep-for 3)))

(When "^I try to turn on importmagic-mode$"
      (lambda ()
        (ignore-errors
          (importmagic-mode))))

(When "^I execute importmagix-fix-imports accepting the first candidate always$"
      (lambda ()
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt vals pred match initial history default)
                     (cond
                      ((string= "Querying for os.path.join: " prompt) "import os.path")
                      ((string= "Querying for os.getcwd: " prompt) "import os")
                      ((string= "Querying for datetime.timedelta: " prompt) "import datetime")
                      ((string= "Querying for datetime.now: " prompt) "import datetime")
                      (t nil)          ; Fail otherwise
                      ))))
          (importmagic-fix-imports))))

(When "^I execute importmagic-fix-symbol-at-point accepting the first candidate$"
      (lambda ()
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt collection &optional predicate require-match initial history default inherit-input-method)
                     "import os")))
          (importmagic-fix-symbol-at-point))))

(When "^I execute importmagic-fix-symbol with argument \"\\([^\"]+\\)\" accepting the first candidate$"
      (lambda (symbol)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt collection &optional predicate require-match initial history default inherit-input-method)
                     "import os")))
          (importmagic-fix-symbol symbol))))


(Then "^importmagic-server should be up$"
      "Asserts that importmagic-server is not nil"
      (lambda ()
        (let ((message "Expected `importmagic-server' to be non-nil, but it was nil."))
          (sleep-for 3)
          (cl-assert importmagic-server nil message))))

(Then "^buffer \"\\([^\"]+\\)\" should have importmagic-server up$"
      (lambda (buffer)
        (with-current-buffer (get-buffer-create buffer)
          (let ((message "Expected `importmagic-server' to be non-nil, but it was nil."))
            (cl-assert importmagic-server nil message)))))

(Then "^importmagic-server should differ between buffers \"\\([^\"]+\\)\" and \"\\([^\"]+\\)\"$"
      (lambda (buffer1 buffer2)
        (let ((server-buffer1 (with-current-buffer
                                  (get-buffer-create buffer1)
                                (epc:uid)))
              (server-buffer2 (with-current-buffer
                                  (get-buffer-create buffer2)
                                (epc:uid))))
          (let ((message "Expected epc uid to be different between buffers, but they were equal"))
            (cl-assert (not (= server-buffer1 server-buffer2)) nil message)))))

(Then "^importmagic-server should not be up$"
      "Asserts that importmagic-server is nil"
      (lambda ()
        (let ((message "Expected `importmagic-server' to be nil, but it was not nil."))
          (sleep-for 3)
          (cl-assert (not importmagic-server) nil message))))

(Then "^an unresolved symbol should be \"\\([^\"]+\\)\"$"
  (lambda (symbol)
    (let ((unresolved (importmagic--get-unresolved-symbols))
          (message (concat "Expected " symbol " to be unresolved but didn't get it")))
      (cl-assert (member symbol unresolved) nil message))))
