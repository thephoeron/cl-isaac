(in-package :cl-isaac/test)

(define-test (cl-isaac api)
  (define-test cl-random-seed
    (is (isaac:init-common-lisp-random-seed))
    (is (isaac:init-common-lisp-random-seed :is64 t))
    (of-type isaac:isaac-ctx (isaac:init-common-lisp-random-seed))
    (of-type isaac:isaac64-ctx (isaac:init-common-lisp-random-seed :is64 t)))
  (skip-on (windows) "Kernel Seeds and Self Seeds are not currently supported on Windows."
    (define-test kernel-seed
      (is (isaac:init-kernel-seed))
      (is (isaac:init-kernel-seed :is64 t))
      (of-type isaac:isaac-ctx (isaac:init-kernel-seed))
      (of-type isaac:isaac64-ctx (isaac:init-kernel-seed :is64 t)))
    (define-test self-seed
      (is (isaac:init-self-seed :count 3))
      (is (isaac:init-self-seed :count 3 :is64 t))
      (of-type isaac:isaac-ctx (isaac:init-self-seed :count 3))
      (of-type isaac:isaac64-ctx (isaac:init-self-seed :count 3 :is64 t)))))
