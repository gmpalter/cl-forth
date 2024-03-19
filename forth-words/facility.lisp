(in-package #:forth)

;;; Facility words as defined in Section 10 of the Forth 2012 specification

;;;---*** AT-XY
;;;---*** KEY?
;;;---*** PAGE


;;; Facility extension words as defined in Section 10 of the Forth 2012 specification

;;;---*** +FIELD

;;;---*** BEGIN-STRUCTURE

;;;---*** CFIELD:

;;;---*** EKEY
;;;---*** EKEY>CHAR
;;;---*** EKEY>FKEY
;;;---*** EKEY?
;;;---*** EMIT?

;;;---*** END-STRUCTURE

;;;---*** FIELD:

;;;---*** K-ALT-MASK, K-CTRL-MASK, K-DELETE, K-DOWN, K-END, K-F1, K-F10, K-F11, K-F12, K-F2, K-F3, K-F4, K-F5
;;;---*** K-F6, K-F7, K-K8, K-F9, K-HOME, K-INSERT, K-LEFT, K-NEXT, K-PRIOR, K-RIGHT, K-SHIFT-MASK, K-UP

(define-word sleep (:word "MS")
  "( u – )"
  "Wait at least U milliseconds"
  (let ((milliseconds (cell-signed (stack-pop data-stack))))
    (when (minusp milliseconds)
      (forth-exception :invalid-numeric-argument "Sleep interval can't be negative in MS"))
    (sleep (/ (float milliseconds 1.0d0) 1000.0d0))))

(define-word date-time (:word "TIME&DATE")
  "( – +n1 +n2 +n3 +n4 +n5 +n6 )"
  "Return the current time and date. +N1 is the second {0. . . 59}, +N2 is the minute {0. . . 59}, +N3 is the hour {0...23},"
  "+N4 is the day {1...31}, +N5 is the month {1...12} and +N6 is the year (e.g., 1991)"
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (stack-push data-stack second)
    (stack-push data-stack minute)
    (stack-push data-stack hour)
    (stack-push data-stack date)
    (stack-push data-stack month)
    (stack-push data-stack year)))

