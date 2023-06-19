(in-package :cl-user)

(defpackage cl-isaac/test
  (:use cl cl-isaac parachute)
  (:export #:*isaac32-ctx*
           #:*isaac64-ctx*))

(in-package :cl-isaac/test)

(defparameter *isaac32-ctx* nil)

(defparameter *isaac64-ctx* nil)
