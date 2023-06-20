(in-package :cl-isaac/test)

(define-test (cl-isaac isaac64)
  (with-fixtures '(*isaac64-ctx*)
    (setf *isaac64-ctx* (isaac:make-isaac64-ctx))))
