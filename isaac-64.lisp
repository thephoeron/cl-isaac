;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: isaac-64.lisp

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014-2022, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package #:cl-isaac)

;; TODO: proof these against ISAAC-64 implementation from http://burtleburtle.net/bob/rand/isaacafa.html

(defstruct isaac64-ctx
  (randcnt 0 :type (unsigned-byte 64))
  (randrsl (make-array 256 :element-type '(unsigned-byte 64) :initial-element 0)
           :type (simple-array (unsigned-byte 64) (256)))
  (randmem (make-array 256 :element-type '(unsigned-byte 64) :initial-element 0)
           :type (simple-array (unsigned-byte 64) (256)))
  (a 0 :type (unsigned-byte 64))
  (b 0 :type (unsigned-byte 64))
  (c 0 :type (unsigned-byte 64)))

(defun generate-next-isaac64-block (ctx)
  ;(declare (optimize (speed 3) (safety 0)))
  (incf (isaac64-ctx-c ctx))
  (incf (isaac64-ctx-b ctx) (isaac64-ctx-c ctx))
  (loop for i from 0 below 256 do
    (setf (isaac64-ctx-a ctx)
          (logxor (isaac64-ctx-a ctx)
                  (the (unsigned-byte 64)
                    (logand #xFFFFFFFFFFFFFFFF
                        (ash (isaac64-ctx-a ctx)
                            (ecase (logand i 3)
                              ((0) 21)
                              ((1) -5)
                              ((2) 12)
                              ((3) -33)))))))
    (setf (isaac64-ctx-a ctx)
          (logand #xFFFFFFFFFFFFFFFF
                  (+ (isaac64-ctx-a ctx)
                     (aref (isaac64-ctx-randmem ctx) (logand (+ i 128) #xFF)))))
    (let* ((x (aref (isaac64-ctx-randmem ctx) i))
           (y (logand #xFFFFFFFFFFFFFFFF
                      (+ (aref (isaac64-ctx-randmem ctx) (logand (ash x -2) #xFF))
                         (isaac64-ctx-a ctx)
                         (isaac64-ctx-b ctx)))))
      (setf (aref (isaac64-ctx-randmem ctx) i) y)
      (setf (isaac64-ctx-b ctx)
            (logand #xFFFFFFFFFFFFFFFF
                    (+ (aref (isaac64-ctx-randmem ctx) (logand (ash y -10) #xFF)) x)))
      (setf (aref (isaac64-ctx-randrsl ctx) i) (isaac64-ctx-b ctx)))))

(defun rand64 (ctx)
  ;;(declare (optimize (speed 3) (safety 0)))
  (cond
    ((zerop (isaac64-ctx-randcnt ctx))
     (generate-next-isaac64-block ctx)
     (setf (isaac64-ctx-randcnt ctx) 255)
     (aref (isaac64-ctx-randrsl ctx) 255))
    (t
     (aref (isaac64-ctx-randrsl ctx)
           (decf (isaac64-ctx-randcnt ctx))))))

(defun rand-bits-64 (ctx n)
  (let ((v 0))
    (loop while (> n 0) do
          (setq v (logior (ash v (min n 64))
                          (logand (1- (ash 1 (min n 64)))
                                  (rand64 ctx))))
          (decf n (min n 64)))
    v))

(defmacro incf-wrap64 (a b)
  `(setf ,a (logand #xFFFFFFFFFFFFFFFF (+ ,a ,b))))

(defmacro decf-wrap64 (a b)
  `(setf ,a (logand #xFFFFFFFFFFFFFFFF (- ,a ,b))))

(defmacro mix64 (a b c d e f g h)
  `(progn
     (decf-wrap64 ,a ,e) (setf ,f (logxor ,f (logand #xFFFFFFFFFFFFFFFF (ash ,h -9)))) (incf-wrap64 ,h ,a)
     (decf-wrap64 ,b ,f) (setf ,g (logxor ,g (logand #xFFFFFFFFFFFFFFFF (ash ,a 9)))) (incf-wrap64 ,a ,b)
     (decf-wrap64 ,c ,g) (setf ,h (logxor ,h (logand #xFFFFFFFFFFFFFFFF (ash ,b -23)))) (incf-wrap64 ,b ,c)
     (decf-wrap64 ,d ,h) (setf ,a (logxor ,a (logand #xFFFFFFFFFFFFFFFF (ash ,c 15)))) (incf-wrap64 ,c ,d)
     (decf-wrap64 ,e ,a) (setf ,b (logxor ,b (logand #xFFFFFFFFFFFFFFFF (ash ,d -14)))) (incf-wrap64 ,d ,e)
     (decf-wrap64 ,f ,b) (setf ,c (logxor ,c (logand #xFFFFFFFFFFFFFFFF (ash ,e 20)))) (incf-wrap64 ,e ,f)
     (decf-wrap64 ,g ,c) (setf ,d (logxor ,d (logand #xFFFFFFFFFFFFFFFF (ash ,f -17)))) (incf-wrap64 ,f ,g)
     (decf-wrap64 ,h ,d) (setf ,e (logxor ,e (logand #xFFFFFFFFFFFFFFFF (ash ,g 14)))) (incf-wrap64 ,g ,h)))

(defun scramble64 (ctx)
  (let (a b c d e f g h)
    ; golden ratio
    (setf a #x9e3779b97f4a7c13  b a  c a  d a  e a  f a  g a  h a)
    ; scramble it
    (loop for i from 0 below 4 do
      (mix64 a b c d e f g h))
    ;; Pass #1
    (loop for i from 0 below 256 by 8 do
      (incf-wrap64 a (aref (isaac64-ctx-randrsl ctx) (+ i 0)))
      (incf-wrap64 b (aref (isaac64-ctx-randrsl ctx) (+ i 1)))
      (incf-wrap64 c (aref (isaac64-ctx-randrsl ctx) (+ i 2)))
      (incf-wrap64 d (aref (isaac64-ctx-randrsl ctx) (+ i 3)))
      (incf-wrap64 e (aref (isaac64-ctx-randrsl ctx) (+ i 4)))
      (incf-wrap64 f (aref (isaac64-ctx-randrsl ctx) (+ i 5)))
      (incf-wrap64 g (aref (isaac64-ctx-randrsl ctx) (+ i 6)))
      (incf-wrap64 h (aref (isaac64-ctx-randrsl ctx) (+ i 7)))
      (mix64 a b c d e f g h)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 0)) a)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 1)) b)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 2)) c)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 3)) d)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 4)) e)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 5)) f)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 6)) g)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 7)) h))
    ;; Pass #2
    (loop for i from 0 below 256 by 8 do
      (incf-wrap64 a (aref (isaac64-ctx-randmem ctx) (+ i 0)))
      (incf-wrap64 b (aref (isaac64-ctx-randmem ctx) (+ i 1)))
      (incf-wrap64 c (aref (isaac64-ctx-randmem ctx) (+ i 2)))
      (incf-wrap64 d (aref (isaac64-ctx-randmem ctx) (+ i 3)))
      (incf-wrap64 e (aref (isaac64-ctx-randmem ctx) (+ i 4)))
      (incf-wrap64 f (aref (isaac64-ctx-randmem ctx) (+ i 5)))
      (incf-wrap64 g (aref (isaac64-ctx-randmem ctx) (+ i 6)))
      (incf-wrap64 h (aref (isaac64-ctx-randmem ctx) (+ i 7)))
      (mix64 a b c d e f g h)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 0)) a)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 1)) b)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 2)) c)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 3)) d)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 4)) e)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 5)) f)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 6)) g)
      (setf (aref (isaac64-ctx-randmem ctx) (+ i 7)) h))
    ; fill in first set
    (generate-next-isaac64-block ctx)
    ; prepare to use first set
    (setf (isaac64-ctx-randcnt ctx) 256)
    ; return CTX
    ctx))

;; EOF
