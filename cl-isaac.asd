;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: cl-isaac.asd

(in-package :cl-user)

(defpackage cl-isaac-asd
  (:use :cl :asdf))

(in-package :cl-isaac-asd)

(defvar *isaac-version* "1.0.1"
  "A string denoting the current version of CL-ISAAC.  Used for diagnostic output.")

(export '*isaac-version*)

(defsystem #:cl-isaac
  :serial t
  :description "Optimized Common Lisp version of Bob Jenkins' ISAAC-32 and ISAAC-64 algorithms, a fast cryptographic random number generator, ready for ASDF and Quicklisp."
  :version #.*isaac-version*
  :author "Doug Hoyte <doug@hoytech.com>"
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "BSD Simplified"
  :components ((:file "packages")
               (:file "isaac-32")
               (:file "isaac-64")
               (:file "cl-isaac")))

;; EOF
