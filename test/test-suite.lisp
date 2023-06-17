(in-package :cl-isaac/test)

(define-test cl-isaac)

(define-test (cl-isaac sanity-check)
  (is = (+ 1 1) 2)
  (is = (* 2 2) 4)
  (is = (length "four") 4)
  (of-type (unsigned-byte 8) #xFF)
  (of-type (unsigned-byte 16) #xFFFF)
  (of-type (unsigned-byte 32) #xFFFFFFFF)
  (of-type (unsigned-byte 64) #xFFFFFFFFFFFFFFFF))