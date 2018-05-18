;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I try to turn on importmagic-mode$"
  (lambda ()
    (ignore-errors
      (importmagic-mode))))

(Given "^buffer is in \\(.+\\)$"
  (lambda (mode)
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

(Then "^importmagic-server should not be up$"
  "Asserts that importmagic-server is nil"
  (lambda ()
    (let ((message "Expected `importmagic-server' to be nil, but it was not nil."))
      (cl-assert (not importmagic-server) nil message))))
