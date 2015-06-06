;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- 
;;;; file: cl-isaac-test.asd

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package :cl-user)

(defpackage cl-isaac-test-asd
  (:use :cl :asdf))

(in-package :cl-isaac-test-asd)

(defsystem #:cl-isaac-test
  :serial t
  :version #.cl-isaac-asd:*isaac-version*
  :description "The test code for CL-ISAAC."
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "BSD Simplified"
  :depends-on (#:cl-isaac
               #:prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-isaac"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

;; EOF
