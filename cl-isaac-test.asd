;;;; file: cl-isaac-test.asd

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
