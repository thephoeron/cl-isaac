;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: cl-isaac.lisp

;; (C) May 2008 Doug Hoyte, HCSW
;; BSD license: you can do anything you want with it (but no warranty).

(in-package #:cl-isaac)

;; Update Seed and Output functions to handle both 32 and 64 bit ISAAC algorithms

(defun init-kernel-seed (&key (is64 nil))
  "Initialize Kernel seed from /dev/arandom on BSD systems, /dev/urandom on Linux systems, or return an error.  If :is64 t, use ISAAC-64 context."
  (let ((ctx (if is64
                 (handler-case (make-isaac64-ctx) (error () nil))
                 (make-isaac-ctx))))
    (if (and is64 ctx)
        (progn
          (or
            (ignore-errors
              (with-open-file (r "/dev/arandom" :direction :input :element-type '(unsigned-byte 64))
                #1=(loop for i from 0 below 256 do
                         (setf (aref (isaac64-ctx-randrsl ctx) i) (read-byte r)))
                t))
            (ignore-errors
              (with-open-file (r "/dev/urandom" :direction :input :element-type '(unsigned-byte 64))
                #1#
                t))
            (error "couldn't open /dev/arandom or /dev/urandom"))
          (scramble64 ctx))
        (progn
          (or
            (ignore-errors
              (with-open-file (r "/dev/arandom" :direction :input :element-type '(unsigned-byte 32))
                #2=(loop for i from 0 below 256 do
                         (setf (aref (isaac-ctx-randrsl ctx) i) (read-byte r)))
                t))
            (ignore-errors
              (with-open-file (r "/dev/urandom" :direction :input :element-type '(unsigned-byte 32))
                #2#
                t))
            (error "couldn't open /dev/arandom or /dev/urandom"))
          (scramble ctx)))))

(defun init-common-lisp-random-seed (&key (is64 nil))
  "Initialize random seed from CL:RANDOM.  If :is64 t, use ISAAC-64 context."
  (let ((ctx (if is64 
                 (handler-case (make-isaac64-ctx) (error () nil))
                 (make-isaac-ctx))))
    (if (and is64 ctx)
        (progn
          (loop for i from 0 below 256 do
                (setf (aref (isaac64-ctx-randrsl ctx) i)
                      (random (ash 1 64))))
          (scramble64 ctx))
        (progn
          (loop for i from 0 below 256 do
                (setf (aref (isaac-ctx-randrsl ctx) i) 
                      (random (ash 1 32))))
          (scramble ctx)))))

(defun init-null-seed (&key (is64 nil))
  "Initialize null seed, useful for testing but not production.  If :is64 t, use ISAAC-64 context."
  (let ((ctx (if is64
                 (handler-case (make-isaac64-ctx) (error () nil))
                 (make-isaac-ctx))))
    (if (and is64 ctx)
        (scramble64 ctx)
        (scramble ctx))))

(defun jenkins-output (filename)
  "Output ISAAC-32 block context to FILENAME using null seed.  Output is the same as Jenkins' randvect.txt"
  (let ((ctx (init-null-seed)))
    (with-open-file (o filename :direction :output :if-exists :supersede)
      (loop for i from 0 below 2 do
        (generate-next-isaac-block ctx)
        (loop for j from 0 below 256 do
          (format o "~(~8,'0x~)" (aref (isaac-ctx-randrsl ctx) j))
          (if (= 7 (logand j 7)) (terpri o)))))))

#+:x86-64
(defun jenkins-output-64 (filename)
  "Output ISAAC-64 block context to FILENAME using null seed."
  (let ((ctx (init-null-seed :is64 t)))
    (with-open-file (o filename :direction :output :if-exists :supersede)
      (loop for i from 0 below 2 do
            (generate-next-isaac64-block ctx)
            (loop for j from 0 below 256 do
                  (format o "~(~16,'0x~)" (aref (issac64-ctx-randrsl ctx) j))
                  (if (= 7 (logand j 7)) (terpri o)))))))

;; EOF
