;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I try to turn on importmagic-mode$"
  (lambda ()
    (ignore-errors
      (importmagic-mode))))

(Given "^buffer is in \\(.+\\)$"
  "Turns on some mode."
  (lambda (mode)
    (let ((v (vconcat [?\C-u ?\M-x] (string-to-vector mode))))
      (execute-kbd-macro v))))

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
