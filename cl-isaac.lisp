;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: cl-isaac.lisp

;; (C) May 2008 Doug Hoyte, HCSW
;; BSD license: you can do anything you want with it (but no warranty).

(in-package #:cl-isaac)

;; Update Seed and Output functions to handle both 32 and 64 bit ISAAC algorithms

(defun init-kernel-seed ()
  (let ((ctx (make-isaac-ctx)))
    (or
      (ignore-errors
        (with-open-file (r "/dev/arandom" :direction :input :element-type '(unsigned-byte 32))
          #1=(loop for i from 0 below 256 do
               (setf (aref (isaac-ctx-randrsl ctx) i) (read-byte r)))
          t))
      (ignore-errors
        (with-open-file (r "/dev/urandom" :direction :input :element-type '(unsigned-byte 32))
          #1#
          t))
      (error "couldn't open /dev/arandom or /dev/urandom"))
    (scramble ctx)))

(defun init-common-lisp-random-seed ()
  (let ((ctx (make-isaac-ctx)))
    (loop for i from 0 below 256 do
      (setf (aref (isaac-ctx-randrsl ctx) i) 
            (random (ash 1 32))))
    (scramble ctx)))

(defun init-null-seed ()
  (let ((ctx (make-isaac-ctx)))
    (scramble ctx)))

;; Output is the same as Jenkins' randvect.txt
(defun jenkins-output (filename)
  (let ((ctx (init-null-seed)))
    (with-open-file (o filename :direction :output :if-exists :supersede)
      (loop for i from 0 below 2 do
        (generate-next-isaac-block ctx)
        (loop for j from 0 below 256 do
          (format o "~(~8,'0x~)" (aref (isaac-ctx-randrsl ctx) j))
          (if (= 7 (logand j 7)) (terpri o)))))))

;; EOF
