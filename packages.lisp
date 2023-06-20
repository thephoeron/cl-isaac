;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: package.lisp

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014--2022, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package :cl-user)

(defpackage #:cl-isaac
  (:nicknames #:isaac)
  (:use :cl :cl-user)
  (:export ;; ISAAC-32
           #:isaac-ctx
           #:isaac-ctx-p
           #:make-isaac-ctx
           #:copy-isaac-ctx
           #:isaac-ctx-randrsl
           #:isaac-ctx-randcnt
           #:isaac-ctx-randmem
           #:isaac-ctx-a
           #:isaac-ctx-b
           #:isaac-ctx-c
           #:generate-next-isaac-block
           #:rand32
           #:rand-bits
           #:mix
           #:scramble
           ;; ISAAC-64
           #:isaac64-ctx
           #:isaac64-ctx-p
           #:make-isaac64-ctx
           #:copy-isaac64-ctx
           #:isaac64-ctx-randrsl
           #:isaac64-ctx-randcnt
           #:isaac64-ctx-randmem
           #:isaac64-ctx-a
           #:isaac64-ctx-b
           #:isaac64-ctx-c
           #:generate-next-isaac64-block
           #:rand64
           #:rand-bits-64
           #:mix64
           #:scramble64
           ;; Common
           #:init-self-seed
           #:init-kernel-seed
           #:init-common-lisp-random-seed
           #:init-null-seed
           #:jenkins-output
           #:jenkins-output-64))

;; EOF
