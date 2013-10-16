;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: package.lisp

(in-package :cl-user)

(defpackage #:cl-isaac
   (:use :cl :cl-user)
   (:export #:init-null-seed
            #:init-kernel-seed
            #:init-common-lisp-random-seed
            #:rand32
            #:rand-bits))
;; EOF
