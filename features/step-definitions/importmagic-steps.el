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
         (importmagic-mode)))

(When "^I try to turn on importmagic-mode$"
      (lambda ()
        (ignore-errors
          (importmagic-mode))))

(Then "^importmagic-server should be up$"
      "Asserts that importmagic-server is not nil"
      (lambda ()
        (let ((message "Expected `importmagic-server' to be non-nil, but it was nil."))
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
          (cl-assert (not importmagic-server) nil message))))

(Then "^an unresolved symbol should be \"\\([^\"]+\\)\"$"
  (lambda (symbol)
    (let ((unresolved (importmagic--get-unresolved-symbols))
          (message (concat "Expected " symbol " to be unresolved but didn't get it")))
      (cl-assert (member symbol unresolved) nil message))))

(When "^I call \"\\(.+\\)\" and accept \"\\([0-9]+\\)\" times?$"
  (lambda (function-name times)
    (execute-kbd-macro
     (kbd (let ((keys (concat "M-x " function-name " RET")))
            (dotimes (_ (string-to-number times))
              (setq keys (concat keys " RET")))
            keys)))))

(When "^I query for symbol \"\\(.+\\)\" accepting the first candidate$"
  (lambda (symbol)
    (execute-kbd-macro
     (kbd (concat "M-x importmagic-fix-symbol RET " symbol " RET")))))

(Then "^I should not see message \"\\(.+\\)\"$"
  "Asserts that MESSAGE has not been printed."
  (lambda (message)
    (let ((msg "Expected '%s' to not be included in the list of printed messages, but it was."))
      (setq message (s-replace "\\\"" "\"" message))
      (cl-assert (not (-contains? (-map 's-trim ecukes-message-log) message)) nil msg message))))

;; Ideally we should assert no error here, but I didn't find a way to
;; do that
(Then "^nothing should happen$"
  (lambda () t))
