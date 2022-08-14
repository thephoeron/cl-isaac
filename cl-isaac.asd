;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*-
;;;; file: cl-isaac.asd

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014--2022, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package :cl-user)

(defpackage cl-isaac-asd
  (:use :cl :asdf)
  (:export #:*isaac-version*))

(in-package :cl-isaac-asd)

(defvar *isaac-version* "1.0.7"
  "A string denoting the current version of CL-ISAAC.  Used for diagnostic output.")

(defsystem #:cl-isaac
  :description "Optimized Common Lisp version of Bob Jenkins' ISAAC-32 and ISAAC-64 algorithms, fast cryptographic random number generators."
  :version #.*isaac-version*
  :author "Doug Hoyte <doug@hoytech.com>"
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
  :homepage "https://thephoeron.common-lisp.dev/cl-isaac"
  :source-control (:git "https://gitlab.common-lisp.net/thephoeron/cl-isaac")
  :bug-tracker "https://gitlab.common-lisp.net/thephoeron/cl-isaac/-/issues"
  :mailto "thephoeron@protonmail.com"
  :license "BSD Simplified"
  :serial t
  :components ((:file "packages")
               (:file "isaac-32")
               (:file "isaac-64")
               (:file "cl-isaac")))

;; EOF
