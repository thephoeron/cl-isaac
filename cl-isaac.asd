;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: cl-isaac.asd

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package :cl-user)

(defpackage cl-isaac-asd
  (:use :cl :asdf))

(in-package :cl-isaac-asd)

(defvar *isaac-version* "1.0.2"
  "A string denoting the current version of CL-ISAAC.  Used for diagnostic output.")

(export '*isaac-version*)

(defsystem #:cl-isaac
  :serial t
  :description "Optimized Common Lisp version of Bob Jenkins' ISAAC-32 and ISAAC-64 algorithms, fast cryptographic random number generators."
  :version #.*isaac-version*
  :author "Doug Hoyte <doug@hoytech.com>"
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "BSD Simplified"
  :components ((:file "packages")
               (:file "isaac-32")
               #+:x86-64 (:file "isaac-64")
               (:file "cl-isaac")))

;; EOF
