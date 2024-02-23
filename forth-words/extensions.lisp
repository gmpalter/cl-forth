(in-package #:forth)

;;; Words not defined in the Forth Standard

(define-word bye ()
  "Exit the Forth interpreter"
  (throw 'bye nil))

(define-word break ()
  "Enter a Lisp debug break loop"
  (break "Debug Break"))
