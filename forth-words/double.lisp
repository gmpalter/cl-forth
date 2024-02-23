(in-package #:forth)

;;; Paragraph numbers refer to the Forth Programmer's Handbook, 3rd Edition

;;; 2.2.1 Arithmetic and Shift Operations

;;; Double-Precision Operations

;;; D+
;;; D-
;;; D2*
;;; D2/

;;; Mixed-Precision Operations

;;; D>S
;;; M*/
;;; M+
;;; M-
;;; M/


;;; 2.2.2 Logical Operations

(define-word abs-double (:word "DABS")
  "( d1 - d2 )"
  "Push the absolute value of D1 onto the data stack"
  (stack-push-double data-stack (abs (stack-pop-double data-stack))))

(define-word max-double (:word "DMAX")
  "( d1 d2 - d3)"
  "Push the larger of D1 and D2 onto the data stack"
  (stack-push-double data-stack (max (stack-pop-double data-stack) (stack-pop-double data-stack))))

(define-word min-double (:word "DMIN")
  "( d1 d2 - d3)"
  "Push the smaller of D1 and D2 onto the data stack"
  (stack-push-double data-stack (min (stack-pop-double data-stack) (stack-pop-double data-stack))))

(define-word negate-double (:word "DNEGATE")
  "( d1 - d2 )"
  "Change the sign of the top of the data stack"
  (stack-push-double data-stack (- (stack-pop-double data-stack))))


;;; 3.6.2 Numeric Output

(define-word print-double-tos (:word "D.")
  "( d - )"
  "Display the top two cells of the data stack as a signed double"
  (format t "~VR. " base (stack-pop-double data-stack)))
