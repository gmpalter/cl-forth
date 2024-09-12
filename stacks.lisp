;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:forth)

(defstruct (stack (:constructor %make-stack) (:print-function %print-stack))
  name
  (cells #() :type (simple-array t (*)))
  (size 0 :type fixnum)
  (depth 0 :type fixnum)
  overflow-key
  underflow-key)

(defun %print-stack (st stream depth)
  (declare (ignore depth))
  (print-unreadable-object (st stream :type t :identity t)
    (format stream "~A (~D/~D cell~:P)" (stack-name st) (stack-depth st) (stack-size st))))

(defun make-stack (name size overflow-key underflow-key)
  (%make-stack :name name :cells (make-array size :initial-element 0) :size size
               :overflow-key overflow-key :underflow-key underflow-key))

(defmacro define-stack-fun (name (st &rest args) &body body)
  (multiple-value-bind (body declarations doc)
      (uiop:parse-body body)
    (declare (ignore doc))
    `(defun ,name (,st ,@args)
       (declare (optimize (speed 3) (safety 0))
                (type stack ,st))
       ,@declarations
       ,@body)))

(declaim (inline stack-overflow-check))
(define-stack-fun stack-overflow-check (st &optional (minimum-space 1))
  (declare (fixnum minimum-space))
  (unless (< (stack-depth st) (the fixnum (- (the fixnum (- (stack-size st) minimum-space)) -1)))
    (forth-exception (stack-overflow-key st))))

(declaim (inline stack-underflow-check))
(define-stack-fun stack-underflow-check (st &optional (minimum-depth 1))
  (declare (fixnum minimum-depth))
  (when (< (stack-depth st) minimum-depth)
    (forth-exception (stack-underflow-key st))))

(declaim (inline stack-cell))
(define-stack-fun stack-cell (st index)
  ;; INDEX is zero-based index of element from top of stack
  (declare (fixnum index))
  (aref (stack-cells st) (the fixnum (- (the fixnum (- (stack-depth st) index)) 1))))

(declaim (inline set-stack-cell))
(define-stack-fun set-stack-cell (st index value)
  (declare (fixnum index))
  (setf (aref (stack-cells st) (the fixnum (- (the fixnum (- (stack-depth st) index)) 1))) value))

(defsetf stack-cell set-stack-cell)

(define-stack-fun stack-reset (st)
  (setf (stack-depth st) 0))

(define-stack-fun stack-push (st value)
  (stack-overflow-check st)
  (prog1
      (setf (aref (stack-cells st) (stack-depth st)) value)
    (setf (stack-depth st) (the fixnum (1+ (stack-depth st))))))

(define-stack-fun stack-push-double (st value)
  (stack-overflow-check st 2)
  (multiple-value-bind (low high)
      (double-components value)
    (setf (aref (stack-cells st) (stack-depth st)) low)
    (setf (aref (stack-cells st) (the fixnum (1+ (stack-depth st)))) high)
    (setf (stack-depth st) (the fixnum (+  (stack-depth st) 2)))
    value))

(define-stack-fun stack-pop (st)
  (stack-underflow-check st)
  (prog1
      (stack-cell st 0)
    (setf (stack-depth st) (the fixnum (1- (stack-depth st))))))

(define-stack-fun stack-pop-double (st)
  (stack-underflow-check st 2)
  (prog1
      (double-cell-signed (stack-cell st 1) (stack-cell st 0))
    (setf (stack-depth st) (the fixnum (-  (stack-depth st) 2)))))

(define-stack-fun stack-pop-double-unsigned (st)
  (stack-underflow-check st 2)
  (prog1
      (double-cell-unsigned (stack-cell st 1) (stack-cell st 0))
    (setf (stack-depth st) (the fixnum (-  (stack-depth st) 2)))))

(define-stack-fun stack-drop (st &optional (n 1))
  "( x(n) ... x(1) -- )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (setf (stack-depth st) (the fixnum (-  (stack-depth st) n))))

(define-stack-fun stack-dup (st)
  "( x -- x x )"
  (stack-underflow-check st)
  (stack-push st (stack-cell st 0)))

(define-stack-fun stack-?dup (st)
  "( x -- 0 | x x )"
  (stack-underflow-check st)
  (unless (zerop (stack-cell st 0))
    (stack-push st (stack-cell st 0))))

(define-stack-fun stack-nip (st)
  "( x1 x2 -- x2 )"
  (stack-underflow-check st 2)
  (prog1
      (setf (stack-cell st 1) (stack-cell st 0))
    (setf (stack-depth st) (the fixnum (1- (stack-depth st))))))

(define-stack-fun stack-over (st)
  "( x1 x2 -- x1 x2 x1 )"
  (stack-underflow-check st 2)
  (stack-push st (stack-cell st 1)))

(define-stack-fun stack-pick (st n)
  "( +n -- x )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (stack-push st (stack-cell st n)))

(define-stack-fun stack-roll (st n)
  "( x(n-1) xn x(n+1) ... x0 -- x(n-1) x(n+1) ... x0 xn )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (let ((cell (stack-cell st n)))
    (loop for i fixnum downfrom (the fixnum (1- n)) to 0
          do (setf (stack-cell st (the fixnum (1+ i))) (stack-cell st i)))
    (setf (stack-cell st 0) cell)))

(define-stack-fun stack-rot (st)
  "( x1 x2 x3 -- x2 x3 x1 )"
  (stack-underflow-check st 3)
  (shiftf (stack-cell st 2) (stack-cell st 1) (stack-cell st 0) (stack-cell st 2))
  nil)
  
(define-stack-fun stack-swap (st)
  "( x1 x2 -- x2 x1 )"
  (stack-underflow-check st 2)
  (shiftf (stack-cell st 1) (stack-cell st 0) (stack-cell st 1))
  nil)

(define-stack-fun stack-tuck (st)
  "( x1 x2 -- x2 x1 x2 )"
  (stack-underflow-check st 2)
  (stack-push st (stack-cell st 0))
  (shiftf (stack-cell st 2) (stack-cell st 1) (stack-cell st 2))
  nil)

(define-stack-fun stack-2drop (st)
  "( x1 x2 -- )"
  (stack-underflow-check st 2)
  (setf (stack-depth st) (the fixnum (-  (stack-depth st) 2))))

(define-stack-fun stack-2dup (st)
  "( x1 x2 -- x1 x2 x1 x2 )"
  (stack-underflow-check st 2)
  (let ((x1 (stack-cell st 1))
        (x2 (stack-cell st 0)))
    (stack-push st x1)
    (stack-push st x2)))

(define-stack-fun stack-2over (st)
  "( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )"
  (stack-underflow-check st 4)
  (let ((x1 (stack-cell st 3))
        (x2 (stack-cell st 2)))
    (stack-push st x1)
    (stack-push st x2)))

(define-stack-fun stack-2rot (st)
  "( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )"
  (stack-underflow-check st 6)
  (shiftf (stack-cell st 4) (stack-cell st 2) (stack-cell st 0) (stack-cell st 4))
  (shiftf (stack-cell st 5) (stack-cell st 3) (stack-cell st 1) (stack-cell st 5))
  nil)

(define-stack-fun stack-2swap (st)
  "( x1 x2 x3 x4 -- x3 x4 x1 x2 )"
  (stack-underflow-check st 4)
  (shiftf (stack-cell st 2) (stack-cell st 0) (stack-cell st 2))
  (shiftf (stack-cell st 1) (stack-cell st 3) (stack-cell st 1))
  nil)

;;; Control flow stack manipulation

(define-stack-fun stack-find-if (predicate st)
  (loop for n fixnum below (stack-depth st)
        when (funcall predicate (stack-cell st n))
          return n))

(define-stack-fun stack-snip (st n)
  "( x(n-1) xn x(n+1) ... x0 -- x(n-1) x(n+1) ... x0 )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (prog1
      (let ((cell (stack-cell st n)))
        (loop for i fixnum downfrom (the fixnum (1- n)) to 0
              do (setf (stack-cell st (1+ i)) (stack-cell st i)))
        cell)
    (setf (stack-depth st) (the fixnum (1- (stack-depth st))))))

;;; FFI callback support

(define-stack-fun stack-contents (st)
  (subseq (stack-cells st) 0 (stack-depth st)))

(define-stack-fun (setf stack-contents) (contents st)
  (let ((depth (length contents)))
    (declare (fixnum depth))
    (replace (stack-cells st) contents :end1 depth)
    (setf (stack-depth st) depth))
  contents)

;;; Display stack contents

(define-stack-fun show-stack (st base)
  (let ((depth (stack-depth st)))
    (declare (fixnum depth))
    (if (zerop depth)
        (format t "~&~A stack empty~%" (stack-name st))
        (progn
          (format t "~&Contents of ~A stack:~%" (string-downcase (stack-name st)))
          (dotimes (i depth)
            (format t "~2D: ~VR~%" i base (aref (stack-cells st) (the fixnum (- (the fixnum (- depth i)) 1)))))))))
