(in-package #:forth)

;;; Words defined by SwiftForth (https://www.forth.com/swiftforth/)

(define-word bye ()
  "Exit the Forth interpreter"
  (throw 'bye nil))

;;; 3.1 Interpreting Source Files

(define-word optional (:word "OPTIONAL")
  "OPTIONAL <name> <description>"
  "If NAME already exists in the LOADED-OPTIONS wordlist, the rest of the file will be ignored; otherwise, NAME"
  "will be added to the LOADED-OPTIONS wordlist and loading will continue. Valid only while including a file."
  (unless (file-input-p files)
    (forth-exception :optional-not-in-file))
  (let ((loaded-options (word-list word-lists "LOADED-OPTIONS" :if-not-found :create))
        (name (word files #\Space)))
    (if (gethash name (dictionary-words loaded-options))
        (flush-input-file files)
        (progn
          (flush-input-line files)
          (setf (gethash name (dictionary-words loaded-options)) (make-word name nil :smudge? t))))))

;;; 3.2 Extended Comments

(define-word extended-comment (:word "{" :immediate? t)
  "Ignore all text up to and including the next right brace"
  "Useful for large comment blocks that contain parentheses (e.g., documenting stack behavior)"
  (word files #\}))

(define-word rest-of-file-comment (:word "\\\\" :immediate? t)
  "Ignore all text in the rest of the file"
  (flush-input-file files))

;;; 3.3 File-Related Debugging Aids

(define-word verbose (:word "VERBOSE")
  "Enables the INCLUDE monitor, with a default behavior of \"display the text of each line.\""
  (setf (files-verbose files) t))

(define-word silent (:word "SILENT")
  "Disables the INCLUDE monitor"
  (setf (files-verbose files) nil)) 

;;; 4.1.1 Dictionary Management

(define-word empty (:word "EMPTY")
  "Reset the dictionary to a predefined golden state, discarding all definitions and releasing all allocated data space"
  "beyond that state. The initial golden state of the dictionary is that following the launch of CL-Forth; this may be"
  "modified using GILD."
  (reset-word-lists word-lists)
  (reset-memory memory))

(define-word gild (:word "GILD")
  "Records the current state of the dictionary as a golden state such that subsequent uses of EMPTY will restore the"
  "dictionary to this state."
  (save-word-lists-state word-lists)
  (save-memory-state memory))

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

