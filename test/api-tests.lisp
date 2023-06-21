(in-package :cl-isaac/test)

(define-test (cl-isaac api)
  (define-test cl-random-seed
    (true (isaac:init-common-lisp-random-seed))
    (true (isaac:init-common-lisp-random-seed :is64 t))
    (of-type isaac:isaac-ctx (isaac:init-common-lisp-random-seed))
    (of-type isaac:isaac64-ctx (isaac:init-common-lisp-random-seed :is64 t)))
  (define-test kernel-seed
    (skip-on (windows) "Kernel Seeds are not currently supported on Windows."
      (true (isaac:init-kernel-seed))
      (true (isaac:init-kernel-seed :is64 t))
      (of-type isaac:isaac-ctx (isaac:init-kernel-seed))
      (of-type isaac:isaac64-ctx (isaac:init-kernel-seed :is64 t))))
  (define-test self-seed
    (skip-on (windows) "Self Seeds are not currently supported on Windows."
      (true (isaac:init-self-seed :count 3))
      (true (isaac:init-self-seed :count 3 :is64 t))
      (of-type isaac:isaac-ctx (isaac:init-self-seed :count 3))
      (of-type isaac:isaac64-ctx (isaac:init-self-seed :count 3 :is64 t)))))
