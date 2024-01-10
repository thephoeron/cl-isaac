;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ISAAC; Base: 10 -*- file: isaac-32.lisp

;; Copyright (c) 2008 Doug Hoyte, HCSW
;; Copyright (c) 2014--2022, "the Phoeron" Colin J.E. Lupton
;; BSD license: you can do anything you want with it (but no warranty).

(in-package #:cl-isaac)

(defstruct isaac-ctx
  (randcnt 0 :type (unsigned-byte 32))
  (randrsl (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0)
           :type (simple-array (unsigned-byte 32) (256)))
  (randmem (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0)
           :type (simple-array (unsigned-byte 32) (256)))
  (a 0 :type (unsigned-byte 32))
  (b 0 :type (unsigned-byte 32))
  (c 0 :type (unsigned-byte 32)))

(defun generate-next-isaac-block (ctx)
  ;(declare (optimize (speed 3) (safety 0)))
  (incf (isaac-ctx-c ctx))
  (incf (isaac-ctx-b ctx) (isaac-ctx-c ctx))
  (loop for i from 0 below 256 do
    (setf (isaac-ctx-a ctx)
          (logxor (isaac-ctx-a ctx)
                  (the (unsigned-byte 32)
                    (logand #xFFFFFFFF
                            (ash (isaac-ctx-a ctx)
                                 (ecase (logand i 3)
                                   ((0) 13)
                                   ((1) -6)
                                   ((2) 2)
                                   ((3) -16)))))))
    (setf (isaac-ctx-a ctx)
          (logand #xFFFFFFFF
                  (+ (isaac-ctx-a ctx)
                     (aref (isaac-ctx-randmem ctx) (logand (+ i 128) #xFF)))))
    (let* ((x (aref (isaac-ctx-randmem ctx) i))
           (y (logand #xFFFFFFFF
                      (+ (aref (isaac-ctx-randmem ctx) (logand (ash x -2) #xFF))
                         (isaac-ctx-a ctx)
                         (isaac-ctx-b ctx)))))
      (setf (aref (isaac-ctx-randmem ctx) i) y)
      (setf (isaac-ctx-b ctx)
            (logand #xFFFFFFFF
                    (+ (aref (isaac-ctx-randmem ctx) (logand (ash y -10) #xFF)) x)))
      (setf (aref (isaac-ctx-randrsl ctx) i) (isaac-ctx-b ctx)))))

(defun rand32 (ctx)
  ;;(declare (optimize (speed 3) (safety 0)))
  (cond
    ((zerop (isaac-ctx-randcnt ctx))
     (generate-next-isaac-block ctx)
     (setf (isaac-ctx-randcnt ctx) 255)
     (aref (isaac-ctx-randrsl ctx) 255))
    (t
     (aref (isaac-ctx-randrsl ctx)
           (decf (isaac-ctx-randcnt ctx))))))

(defun rand-bits (ctx n)
  (let ((v 0))
    (loop while (> n 0) do
      (setq v (logior (ash v (min n 32))
                      (logand (1- (ash 1 (min n 32)))
                              (rand32 ctx))))
      (decf n (min n 32)))
    v))

(defmacro incf-wrap32 (a b)
  `(setf ,a (logand #xFFFFFFFF (+ ,a ,b))))

(defmacro mix (a b c d e f g h)
  `(progn
     (setf ,a (logxor ,a (logand #xFFFFFFFF (ash ,b 11)))) (incf-wrap32 ,d ,a) (incf-wrap32 ,b ,c)
     (setf ,b (logxor ,b (logand #xFFFFFFFF (ash ,c -2)))) (incf-wrap32 ,e ,b) (incf-wrap32 ,c ,d)
     (setf ,c (logxor ,c (logand #xFFFFFFFF (ash ,d 8)))) (incf-wrap32 ,f ,c) (incf-wrap32 ,d ,e)
     (setf ,d (logxor ,d (logand #xFFFFFFFF (ash ,e -16)))) (incf-wrap32 ,g ,d) (incf-wrap32 ,e ,f)
     (setf ,e (logxor ,e (logand #xFFFFFFFF (ash ,f 10)))) (incf-wrap32 ,h ,e) (incf-wrap32 ,f ,g)
     (setf ,f (logxor ,f (logand #xFFFFFFFF (ash ,g -4)))) (incf-wrap32 ,a ,f) (incf-wrap32 ,g ,h)
     (setf ,g (logxor ,g (logand #xFFFFFFFF (ash ,h 8)))) (incf-wrap32 ,b ,g) (incf-wrap32 ,h ,a)
     (setf ,h (logxor ,h (logand #xFFFFFFFF (ash ,a -9)))) (incf-wrap32 ,c ,h) (incf-wrap32 ,a ,b)))

(defun scramble (ctx)
  (let (a b c d e f g h)
    (setf a #x9e3779b9  b a  c a  d a  e a  f a  g a  h a) ; golden ratio

    (loop for i from 0 below 4 do
      (mix a b c d e f g h))

    ;; Pass #1
    (loop for i from 0 below 256 by 8 do
      (incf-wrap32 a (aref (isaac-ctx-randrsl ctx) (+ i 0)))
      (incf-wrap32 b (aref (isaac-ctx-randrsl ctx) (+ i 1)))
      (incf-wrap32 c (aref (isaac-ctx-randrsl ctx) (+ i 2)))
      (incf-wrap32 d (aref (isaac-ctx-randrsl ctx) (+ i 3)))
      (incf-wrap32 e (aref (isaac-ctx-randrsl ctx) (+ i 4)))
      (incf-wrap32 f (aref (isaac-ctx-randrsl ctx) (+ i 5)))
      (incf-wrap32 g (aref (isaac-ctx-randrsl ctx) (+ i 6)))
      (incf-wrap32 h (aref (isaac-ctx-randrsl ctx) (+ i 7)))
      (mix a b c d e f g h)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 0)) a)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 1)) b)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 2)) c)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 3)) d)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 4)) e)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 5)) f)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 6)) g)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 7)) h))

    ;; Pass #2
    (loop for i from 0 below 256 by 8 do
      (incf-wrap32 a (aref (isaac-ctx-randmem ctx) (+ i 0)))
      (incf-wrap32 b (aref (isaac-ctx-randmem ctx) (+ i 1)))
      (incf-wrap32 c (aref (isaac-ctx-randmem ctx) (+ i 2)))
      (incf-wrap32 d (aref (isaac-ctx-randmem ctx) (+ i 3)))
      (incf-wrap32 e (aref (isaac-ctx-randmem ctx) (+ i 4)))
      (incf-wrap32 f (aref (isaac-ctx-randmem ctx) (+ i 5)))
      (incf-wrap32 g (aref (isaac-ctx-randmem ctx) (+ i 6)))
      (incf-wrap32 h (aref (isaac-ctx-randmem ctx) (+ i 7)))
      (mix a b c d e f g h)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 0)) a)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 1)) b)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 2)) c)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 3)) d)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 4)) e)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 5)) f)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 6)) g)
      (setf (aref (isaac-ctx-randmem ctx) (+ i 7)) h))

    (generate-next-isaac-block ctx)
    (setf (isaac-ctx-randcnt ctx) 256)

    ctx))

;; EOF
