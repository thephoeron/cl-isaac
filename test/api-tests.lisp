(in-package :cl-isaac/test)

(define-test (cl-isaac api)
  (define-test kernel-seed
    (is (isaac:init-kernel-seed :is64 t))
    (is (isaac:init-kernel-seed :is64 nil))
    (of-type isaac:isaac64-ctx (isaac:init-kernel-seed :is64 t))
    (of-type isaac:isaac32-ctx (isaac:init-kernel-seed :is64 nil))))
