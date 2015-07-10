;;;; file: t/cl-isaac.lisp

(in-package :cl-user)

(defpackage cl-isaac-test
  (:use cl cl-isaac prove))

(in-package :cl-isaac-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-isaac)' in your Lisp.

(plan 5)

;; Sanity Test

(deftest sanity-check
  (pass "Prove is loaded and ready to go.")
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4.")
  (is (length "four")
      4
      "Length: (length \"four\") => 4."))

;; Self-seed Context Tests

(defparameter *ssctx32* (isaac:init-self-seed :count 5))

(defparameter *ssctx64* (isaac:init-self-seed :count 5 :is64 t))

(deftest self-seed
  (is-type *ssctx32*
           'cl-isaac:isaac-ctx
           "*SSCTX32* has a type of ISAAC-CTX.")
  (ok (<= (integer-length (isaac:rand32 *ssctx32*))
          32)
      "Generated a random 32-bit integer with RAND32 on *SSCTX32*.")
  (is-type *ssctx64*
           'cl-isaac:isaac64-ctx
           "*SSCTX64* has a type of ISAAC64-CTX.")
  (ok (<= (integer-length (isaac:rand64 *ssctx64*))
          64)
      "Generated a random 64-bit integer with RAND64 on *SSCTX64*."))

;; Kernel-seed Context Tests

(defparameter *ksctx32* (isaac:init-kernel-seed))

(defparameter *ksctx64* (isaac:init-kernel-seed :is64 t))

(deftest kernel-seed
  (is-type *ksctx32*
           'cl-isaac:isaac-ctx
           "*KSCTX32* has a type of ISAAC-CTX.")
  (ok (<= (integer-length (isaac:rand32 *ksctx32*))
          32)
      "Generated a random 32-bit integer with RAND32 on *KSCTX32*.")
  (is-type *ksctx64*
           'cl-isaac:isaac64-ctx
           "*KSCTX64* has a type of ISAAC64-CTX.")
  (ok (<= (integer-length (isaac:rand64 *ksctx64*))
          64)
      "Generated a random 64-bit integer with RAND64 on *KSCTX64*."))

;; Common Lisp RANDOM seed Context Tests

(defparameter *rsctx32* (isaac:init-common-lisp-random-seed))

(defparameter *rsctx64* (isaac:init-common-lisp-random-seed :is64 t))

(deftest common-lisp-random-seed
  (is-type *rsctx32*
           'cl-isaac:isaac-ctx
           "*RSCTX32* has a type of ISAAC-CTX.")
  (ok (<= (integer-length (isaac:rand32 *rsctx32*))
          32)
      "Generated a random 32-bit integer with RAND32 on *RSCTX32*.")
  (is-type *rsctx64*
           'cl-isaac:isaac64-ctx
           "*RSCTX64* has a type of ISAAC64-CTX.")
  (ok (<= (integer-length (isaac:rand64 *rsctx64*))
          64)
      "Generated a random 64-bit integer with RAND64 on *RSCTX64*."))

;; Jenkins' Tests

(deftest jenkins-outputs
  (ok (not (isaac:jenkins-output "jenkins.txt"))
      "Created file jenkins.txt")
  (is (with-open-file (o "jenkins.txt")
        (file-length o))
      4160
      "Jenkins' 32-bit test output is the correct length.")
  (ok (not (isaac:jenkins-output-64 "jenkins64.txt"))
      "Created file jenkins64.txt")
  (is (with-open-file (o "jenkins64.txt")
        (file-length o))
      8256
      "Jenkins' 64-bit test output is the correct length."))

(run-test-all)

;; EOF
