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

(defun %print-stack (stack stream depth)
  (declare (ignore depth))
  (print-unreadable-object (stack stream :type t :identity t)
    (format stream "~A (~D/~D cell~:P)" (stack-name stack) (stack-depth stack) (stack-size stack))))

(defun make-stack (name size overflow-key underflow-key)
  (%make-stack :name name :cells (make-array size :initial-element 0) :size size
               :overflow-key overflow-key :underflow-key underflow-key))

;;; NOTE: The stack argument to functions defined by this macro MUST be named STACK.
(defmacro define-stack-fun (name arglist &body body)
  (multiple-value-bind (body declarations doc)
      (uiop:parse-body body)
    (declare (ignore doc))
    `(defun ,name (,@arglist)
       (declare (optimize (speed 3) (safety 0))
                (type stack stack))
       ,@declarations
       ,@body)))

(declaim (inline stack-overflow-check))
(define-stack-fun stack-overflow-check (stack &optional (minimum-space 1))
  (declare (fixnum minimum-space))
  (unless (< (stack-depth stack) (the fixnum (- (the fixnum (- (stack-size stack) minimum-space)) -1)))
    (forth-exception (stack-overflow-key stack))))

(declaim (inline stack-underflow-check))
(define-stack-fun stack-underflow-check (stack &optional (minimum-depth 1))
  (declare (fixnum minimum-depth))
  (when (< (stack-depth stack) minimum-depth)
    (forth-exception (stack-underflow-key stack))))

(declaim (inline stack-cell))
(define-stack-fun stack-cell (stack index)
  ;; INDEX is zero-based index of element from top of stack
  (declare (fixnum index))
  (aref (stack-cells stack) (the fixnum (- (the fixnum (- (stack-depth stack) index)) 1))))

(declaim (inline set-stack-cell))
(define-stack-fun set-stack-cell (stack index value)
  (declare (fixnum index))
  (setf (aref (stack-cells stack) (the fixnum (- (the fixnum (- (stack-depth stack) index)) 1))) value))

(defsetf stack-cell set-stack-cell)

(define-stack-fun stack-reset (stack)
  (setf (stack-depth stack) 0))

(define-stack-fun stack-push (stack value)
  (stack-overflow-check stack)
  (prog1
      (setf (aref (stack-cells stack) (stack-depth stack)) value)
    (setf (stack-depth stack) (the fixnum (1+ (stack-depth stack))))))

(define-stack-fun stack-push-double (stack value)
  (stack-overflow-check stack 2)
  (multiple-value-bind (low high)
      (double-components value)
    (setf (aref (stack-cells stack) (stack-depth stack)) low)
    (setf (aref (stack-cells stack) (the fixnum (1+ (stack-depth stack)))) high)
    (setf (stack-depth stack) (the fixnum (+  (stack-depth stack) 2)))
    value))

(define-stack-fun stack-pop (stack)
  (stack-underflow-check stack)
  (prog1
      (stack-cell stack 0)
    (setf (stack-depth stack) (the fixnum (1- (stack-depth stack))))))

(define-stack-fun stack-pop-double (stack)
  (stack-underflow-check stack 2)
  (prog1
      (double-cell-signed (stack-cell stack 1) (stack-cell stack 0))
    (setf (stack-depth stack) (the fixnum (-  (stack-depth stack) 2)))))

(define-stack-fun stack-pop-double-unsigned (stack)
  (stack-underflow-check stack 2)
  (prog1
      (double-cell-unsigned (stack-cell stack 1) (stack-cell stack 0))
    (setf (stack-depth stack) (the fixnum (-  (stack-depth stack) 2)))))

(define-stack-fun stack-drop (stack)
  "( x1 -- )"
  (stack-underflow-check stack)
  (setf (stack-depth stack) (the fixnum (1- (stack-depth stack)))))

(define-stack-fun stack-dup (stack)
  "( x -- x x )"
  (stack-underflow-check stack)
  (stack-push stack (stack-cell stack 0)))

(define-stack-fun stack-?dup (stack)
  "( x -- 0 | x x )"
  (stack-underflow-check stack)
  (unless (zerop (stack-cell stack 0))
    (stack-push stack (stack-cell stack 0))))

(define-stack-fun stack-nip (stack)
  "( x1 x2 -- x2 )"
  (stack-underflow-check stack 2)
  (prog1
      (setf (stack-cell stack 1) (stack-cell stack 0))
    (setf (stack-depth stack) (the fixnum (1- (stack-depth stack))))))

(define-stack-fun stack-over (stack)
  "( x1 x2 -- x1 x2 x1 )"
  (stack-underflow-check stack 2)
  (stack-push stack (stack-cell stack 1)))

(define-stack-fun stack-pick (stack n)
  "( +n -- x )"
  (declare (fixnum n))
  (stack-underflow-check stack n)
  (stack-push stack (stack-cell stack n)))

(define-stack-fun stack-roll (stack n)
  "( x(n-1) xn x(n+1) ... x0 -- x(n-1) x(n+1) ... x0 xn )"
  (declare (fixnum n))
  (stack-underflow-check stack n)
  (let ((cell (stack-cell stack n)))
    (loop for i fixnum downfrom (the fixnum (1- n)) to 0
          do (setf (stack-cell stack (the fixnum (1+ i))) (stack-cell stack i)))
    (setf (stack-cell stack 0) cell)))

(define-stack-fun stack-rot (stack)
  "( x1 x2 x3 -- x2 x3 x1 )"
  (stack-underflow-check stack 3)
  (shiftf (stack-cell stack 2) (stack-cell stack 1) (stack-cell stack 0) (stack-cell stack 2))
  nil)
  
(define-stack-fun stack-rot-down (stack)
  "( x1 x2 x3 -- x3 x1 x2 )"
  (stack-underflow-check stack 3)
  (shiftf (stack-cell stack 2) (stack-cell stack 0) (stack-cell stack 1) (stack-cell stack 2))
  nil)

(define-stack-fun stack-swap (stack)
  "( x1 x2 -- x2 x1 )"
  (stack-underflow-check stack 2)
  (shiftf (stack-cell stack 1) (stack-cell stack 0) (stack-cell stack 1))
  nil)

(define-stack-fun stack-tuck (stack)
  "( x1 x2 -- x2 x1 x2 )"
  (stack-underflow-check stack 2)
  (stack-push stack (stack-cell stack 0))
  (shiftf (stack-cell stack 2) (stack-cell stack 1) (stack-cell stack 2))
  nil)

(define-stack-fun stack-2drop (stack)
  "( x1 x2 -- )"
  (stack-underflow-check stack 2)
  (setf (stack-depth stack) (the fixnum (- (stack-depth stack) 2))))

(define-stack-fun stack-2dup (stack)
  "( x1 x2 -- x1 x2 x1 x2 )"
  (stack-underflow-check stack 2)
  (let ((x1 (stack-cell stack 1))
        (x2 (stack-cell stack 0)))
    (stack-push stack x1)
    (stack-push stack x2)))

(define-stack-fun stack-2over (stack)
  "( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )"
  (stack-underflow-check stack 4)
  (let ((x1 (stack-cell stack 3))
        (x2 (stack-cell stack 2)))
    (stack-push stack x1)
    (stack-push stack x2)))

(define-stack-fun stack-2rot (stack)
  "( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )"
  (stack-underflow-check stack 6)
  (shiftf (stack-cell stack 4) (stack-cell stack 2) (stack-cell stack 0) (stack-cell stack 4))
  (shiftf (stack-cell stack 5) (stack-cell stack 3) (stack-cell stack 1) (stack-cell stack 5))
  nil)

(define-stack-fun stack-2swap (stack)
  "( x1 x2 x3 x4 -- x3 x4 x1 x2 )"
  (stack-underflow-check stack 4)
  (shiftf (stack-cell stack 2) (stack-cell stack 0) (stack-cell stack 2))
  (shiftf (stack-cell stack 1) (stack-cell stack 3) (stack-cell stack 1))
  nil)

;;; Control flow stack manipulation

(define-stack-fun stack-find-if (predicate stack)
  (loop for n fixnum below (stack-depth stack)
        when (funcall predicate (stack-cell stack n))
          return n))

(define-stack-fun stack-snip (stack n)
  "( x(n-1) xn x(n+1) ... x0 -- x(n-1) x(n+1) ... x0 )"
  (declare (fixnum n))
  (stack-underflow-check stack n)
  (prog1
      (let ((cell (stack-cell stack n)))
        (loop for i fixnum downfrom (the fixnum (1- n)) to 0
              do (setf (stack-cell stack (1+ i)) (stack-cell stack i)))
        cell)
    (setf (stack-depth stack) (the fixnum (1- (stack-depth stack))))))

;;; FFI callback support

(define-stack-fun stack-contents (stack)
  (subseq (stack-cells stack) 0 (stack-depth stack)))

(define-stack-fun (setf stack-contents) (contents stack)
  (let ((depth (length contents)))
    (declare (fixnum depth))
    (replace (stack-cells stack) contents :end1 depth)
    (setf (stack-depth stack) depth))
  contents)

;;; Optimizer support

;;; Marker to instruct the optimizer to flush its knowledge of the stack contents
(defmacro flush-optimizer-stack (&key count contains)
  (declare (ignore count contains))
  nil)

(define-stack-fun stack-roll-down (stack n)
  "( xn ... x1 x0 -- x0 xn ... x1 )"
  (declare (fixnum n))
  (let ((depth (stack-depth stack))
        (cell (stack-cell stack 0)))
    (replace (stack-cells stack) (stack-cells stack) :start1 (- depth n) :end1 depth
                                                     :start2 (- depth n 1) :end2 (1- depth))
    (setf (stack-cell stack n) cell)))


;;; Display stack contents

(define-stack-fun show-stack (stack base)
  (let ((depth (stack-depth stack)))
    (declare (fixnum depth))
    (if (zerop depth)
        (format t "~&~A stack empty~%" (stack-name stack))
        (progn
          (format t "~&Contents of ~A stack:~%" (string-downcase (stack-name stack)))
          (dotimes (i depth)
            (format t "~2D: ~VR~%" i base (aref (stack-cells stack) (the fixnum (- (the fixnum (- depth i)) 1)))))))))
