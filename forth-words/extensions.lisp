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

(define-word rest-of-file-comment (:word "\\\\")
  "Ignore all text in the rest of the file"
  (flush-input-file files))

;;; 3.3 File-Related Debugging Aids

;;; VERBOSE
;;; SILENT

;;; 4.1.1 Dictionary Management

;;; EMPTY

;;; 4.2.2 Detecting Name Conflicts

(define-word redefinition-warnings (:word "WARNING")
  "( - addr )"
  "Return the address of the flag that controls compiler redefinition warnings"
  (stack-push data-stack (state-slot-address memory 'show-redefinition-warnings?)))

(define-word set-flag (:word "ON")
  "( addr - )"
  "Set the flag at ADDR to true"
  (setf (memory-byte memory (stack-pop data-stack)) +true+))

(define-word clear-flag (:word "OFF")
  "( addr - )"
  "Set the flag at ADDR to false"
  (setf (memory-byte memory (stack-pop data-stack)) +false+))

(define-word suppress-one-redefinition-warning (:word "-?")
  "Suppress redefinition warnings for the next definition only"
  (setf show-redefinition-warnings? +false+
        reset-redefinition-warnings? t))


;;; Words not defined in either Standard Forth or SwiftForth

(define-word break (:immediate? t)
  "Enter a Lisp debug break loop"
  (break "Debug Break"))

(define-word show-definition-code (:word "SHOW-CODE")
  "( - addr )"
  "Return the address of the flag that controls whether to show the code generated when a definition is defined"
  (stack-push data-stack (state-slot-address memory 'show-definition-code?)))

(define-word show-all-words (:word "ALL-WORDS" :inlineable? nil)
  "List all the definition names in all word lists of the search order"
  (dolist (dictionary (word-lists-search-order word-lists))
    (show-words dictionary)))

