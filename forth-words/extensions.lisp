(in-package #:forth)

;;; Words defined by SwiftForth (https://www.forth.com/swiftforth/)

(define-word bye ()
  "Exit the Forth interpreter"
  (throw 'bye nil))

;;; 3.1 Interpreting Source Files

;;; OPTIONAL

;;; 3.2 Extended Comments

(define-word extended-comment (:word "{")
  "Ignore all text up to and including the next right brace"
  "Useful for large comment blocks that contain parentheses (e.g., documenting stack behavior)"
  (word files #\}))

;;; \\

;;; 3.3 File-Related Debugging Aids

;;; VERBOSE
;;; SILENT

;;; 4.2.2 Detecting Name Conflicts

;;; WARNING
;;; ON
;;; OFF
;;; -?


;;; Words not defined in either Standard Forth or SwiftForth

(define-word break (:immediate? t)
  "Enter a Lisp debug break loop"
  (break "Debug Break"))
