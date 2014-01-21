;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: isaac-64.lisp

(in-package #:cl-isaac)

;; TODO: proof these against ISAAC-64 implementation from http://burtleburtle.net/bob/rand/isaacafa.html

(defstruct isaac64-ctx
  (randcnt 0 :type (unsigned-byte 64))
  (randrsl (make-array 256 :element-type '(unsigned-byte 64) :initial-element 0)
           :type (simple-array (unsigned-byte 64) (256)))
  (randmem (make-array 256 :element-type '(unsigned-byte 64) :initial-element 0)
           :type (simple-array (unsigned-byte 64) (256)))
  (a 0 :type (unsigned-byte 64))
  (b 0 :type (unsigned-byte 64))
  (c 0 :type (unsigned-byte 64)))

;; EOF
