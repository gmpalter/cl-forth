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

;;; Double-Number words as defined in Section 8 of the Forth 2012 specification

(define-word 2constant (:word "2CONSTANT")
  "2CONSTANT <name>" "( x1 x2 -- )"
  "Create a dictionary entry for <name> which pushes the signed double integer X1,X2 on the data stack"
  (let ((name (word files #\Space))
        (value (stack-pop-double data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'push-parameter-as-double-cell :parameters (list value))))
      (setf (word-inline-forms word) `((stack-push-double data-stack ,value))
            (word-inlineable? word) t)
      (add-and-register-word fs word))))

(define-word double-literal (:word "2LITERAL" :immediate? t :compile-only? t)
  "( x1 x2 -- )"
  "Compile X1 and X2 into the current definition. When executed, push X1 and X2 onto the data stack"
  (let ((value (stack-pop-double-unsigned data-stack)))
    (add-to-definition fs
      `(stack-push-double data-stack ,value))))

(define-word 2variable (:word "2VARIABLE")
  "2VARIABLE <name>"
  "Allocate two cells in data space and create a dictionary entry for <name> which returns the address of the first cell"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (allocate-memory memory (* 2 +cell-size+)))
           (word (make-word name #'push-parameter-as-cell :parameters (list address) :created-word? t)))
      (setf (word-inline-forms word) `((stack-push data-stack ,address))
            (word-inlineable? word) t)
      (add-and-register-word fs word address))))

(define-word add-double (:word "D+")
  "( d1 d2 -- d3 )"
  (stack-push-double data-stack (+ (stack-pop-double data-stack) (stack-pop-double data-stack))))

(define-word subtract-double (:word "D-")
  "( d1 d2 -- d3 )"
  (let ((d2 (stack-pop-double data-stack))
        (d1 (stack-pop-double data-stack)))
    (stack-push-double data-stack (- d1 d2))))

(define-word print-double-tos (:word "D.")
  "( d -- )"
  "Display the top two cells of the data stack as a signed double in the current base"
  (format t "~VR " base (stack-pop-double data-stack)))

(define-word print-double-tos-in-field (:word "D.R")
  "( d +n -- )"
  "Display D right aligned in a field N characters wide. If the number of characters required to display D"
  "is greater than N, all digits are displayed in a field as wide as necessary with no leading spaces"
  (let ((width (stack-pop data-stack))
        (value (stack-pop-double data-stack)))
    (when (minusp width)
      (forth-exception :invalid-numeric-argument "Field width to D.R can't be negative"))
    (format t "~V,VR" base width value)))

(define-word minusp-double (:word "D0<")
  " ( d -- flag )"
  "Return true if D is less than zero"
  (stack-push data-stack (if (minusp (stack-pop-double data-stack)) +true+ +false+)))

(define-word zerop-double (:word "D0=")
  " ( d -- flag )"
  "Return true if D is equal to zero"
  (stack-push data-stack (if (zerop (stack-pop-double data-stack)) +true+ +false+)))

(define-word ash-left-1-double (:word "D2*")
  "( d1 -- d2 )"
  (stack-push-double data-stack (ash (stack-pop-double data-stack) 1)))

(define-word ash-right-1-double (:word "D2/")
  "( d1 -- d2)"
  (stack-push-double data-stack (ash (stack-pop-double data-stack) -1)))

(define-word less-than-double (:word "D<")
  " ( d1 d2 -- flag )"
  "Return true if D1 is less than D2"
  ;; As the first value popped off the stack is D2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (> (stack-pop-double data-stack) (stack-pop-double data-stack)) +true+ +false+)))

(define-word equal-double (:word "D=")
  " ( d1 d2 -- flag )"
  "Return true if D1 is equal to D2"
  (stack-push data-stack (if (= (stack-pop-double data-stack) (stack-pop-double data-stack)) +true+ +false+)))

(define-word double-to-single (:word "D>S")
  "( d -- n )"
  "Convert the signed double integer D to a signed single integer N"
  (stack-push data-stack (cell-signed (stack-pop-double data-stack))))

(define-word abs-double (:word "DABS")
  "( d1 -- d2 )"
  "Push the absolute value of D1 onto the data stack"
  (stack-push-double data-stack (abs (stack-pop-double data-stack))))

(define-word max-double (:word "DMAX")
  "( d1 d2 -- d3)"
  "Push the larger of D1 and D2 onto the data stack"
  (stack-push-double data-stack (max (stack-pop-double data-stack) (stack-pop-double data-stack))))

(define-word min-double (:word "DMIN")
  "( d1 d2 -- d3)"
  "Push the smaller of D1 and D2 onto the data stack"
  (stack-push-double data-stack (min (stack-pop-double data-stack) (stack-pop-double data-stack))))

(define-word negate-double (:word "DNEGATE")
  "( d1 -- d2 )"
  "Change the sign of the top of the data stack"
  (stack-push-double data-stack (- (stack-pop-double data-stack))))

(define-word multiply-divide-double-single-single (:word "M*/")
  "( d1 n1 +n2 -- d2 )"
  "Multiple the double D by the single N1, divide by the positive N2, producing the double quotient D2"
  (let ((n2 (cell-signed (stack-pop data-stack)))
        (n1 (cell-signed (stack-pop data-stack)))
        (d1 (stack-pop-double data-stack)))
    (unless (plusp n2)
      (forth-exception :invalid-numeric-argument))
    (stack-push-double data-stack (truncate (* d1 n1) n2))))

(define-word add-double-single (:word "M+")
  "( d1 n -- d2 )"
  "Add the double D1 and the single N, producing the double result D2"
  (stack-push-double data-stack (+ (cell-signed (stack-pop data-stack)) (stack-pop-double data-stack))))


;;; Double-Number extension words as defined in Section 8 of the Forth 2012 specification

(define-word stack-2rot (:word "2ROT")
  "( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )"
  "Rotate the top three cell pairs on the data stack, bringing the pair X1 X2 to the top"
  (stack-2rot data-stack))

(define-word 2value (:word "2VALUE")
  "2VALUE <name>" "( x1 x2 -- )"
  "Allocate two cells in data space, initialize it to X1 X2, and create a dictionary entry for <name> which returns"
  "the contents of those cells in data space. To change the value, use TO"
  (let ((name (word files #\Space))
        (value (stack-pop-double data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (allocate-memory memory (* 2 +cell-size+)))
           (word (make-word name #'push-value :parameters (list address :2value) :created-word? t)))
      (setf (memory-double-cell memory address) value)
      (add-and-register-word fs word address))))

(define-word less-than-double-unsigned (:word "DU<")
  " ( du1 du2 -- flag )"
  "Return true if DU1 is less than DU2"
  ;; As the first value popped off the stack is DU2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (> (stack-pop-double-unsigned data-stack) (stack-pop-double-unsigned data-stack))
                             +true+ +false+)))
