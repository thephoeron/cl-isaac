(in-package :cl-isaac/test)

(define-test (cl-isaac isaac32)
  (with-fixtures '(*isaac32-ctx*)
    (setf *isaac32-ctx* (isaac:make-isaac-ctx))))
