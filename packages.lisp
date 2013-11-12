;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: package.lisp

(in-package :cl-user)

(defpackage #:cl-isaac
  (:nicknames #:isaac)
  (:use :cl :cl-user)
  (:export #:isaac-ctx
           #:generate-next-isaac-block
           #:rand32
           #:rand-bits
           #:mix
           #:scramble
           #:init-kernel-seed
           #:init-common-lisp-random-seed
           #:init-null-seed
           #:jenkins-output))

;; EOF
