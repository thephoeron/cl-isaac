;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*-
;;;; file: cl-isaac.asd

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014--2023, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package :cl-user)

(defpackage cl-isaac-asd
  (:use :cl :asdf)
  (:export #:*isaac-version*))

(in-package :cl-isaac-asd)

(defsystem cl-isaac
  :description "Optimized Common Lisp version of Bob Jenkins' ISAAC-32 and ISAAC-64 algorithms, fast cryptographic random number generators."
  :version (:read-file-form "VERSION")
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
               (:file "cl-isaac"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-isaac/test))))

(defsystem cl-isaac/test
  :description "Parachute test suite for CL-ISAAC."
  :version (:read-file-form "VERSION")
  :author "\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
  :homepage "https://thephoeron.common-lisp.dev/cl-isaac"
  :source-control (:git "https://gitlab.common-lisp.net/thephoeron/cl-isaac")
  :bug-tracker "https://gitlab.common-lisp.net/thephoeron/cl-isaac/-/issues"
  :mailto "thephoeron@protonmail.com"
  :license "BSD Simplified"
  :depends-on (:cl-isaac :parachute :trivial-features)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test-suite")
                             (:file "isaac32-tests")
                             (:file "isaac64-tests")
                             (:file "api-tests")
                             (:file "report"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :cl-isaac/test)))

(defparameter *isaac-version* (asdf:component-version (asdf:find-system :cl-isaac))
  "A string denoting the current version of CL-ISAAC. Used for diagnostic output.")

;; EOF
