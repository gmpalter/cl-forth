;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://github.com/gmpalter/cl-forth/tree/main?tab=MIT-1-ov-file#readme

(in-package #:forth)

;;; Words defined as "Common Usage" in the Forth Programmer's Handbook, 3rd Edition

(define-word allocate-counted-string (:word ",\"")
  ",\" <text>\""
  "Compile TEXT as a counted string. User is responsible for keeping track of its address in data space"
  (let* ((text (parse files #\"))
         (text-size (* (length text) +char-size+))
         ;; Length of a counted string is always a single byte regardless of character size
         (address (allocate-memory memory (1+ text-size))))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address (1+ text-size))
      (native-into-forth-counted-string text forth-memory offset))))

(define-word add-two (:word "2+")
  "( n1 - n2 )"
  (stack-push data-stack (cell-signed (+ (cell-signed (stack-pop data-stack)) 2))))

(define-word subtract-two (:word "2-")
  "( n1 - n2 )"
  (stack-push data-stack (cell-signed (- (cell-signed (stack-pop data-stack)) 2))))

(define-word incf-char (:word "C+!")
  "( char a-addr - )"
  "Add CHAR to the contents of the character at A-ADDR and store the result back into A-ADDR"
  (let ((address (stack-pop data-stack))
        (char (extract-char (stack-pop data-stack))))
    (setf (memory-char memory address) (extract-char (+ (memory-char memory address) char)))))

(define-state-word context :word "CONTEXT")

(define-state-word current :word "CURRENT")

(define-word cvariable (:word "CVARIABLE")
  "CVARIABLE <name>"
  "Allocate space for a character (currently 1 byte) in data space and create a dictionary entry"
  "for <name> which returns the address of that character"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((address (allocate-memory memory +char-size+))
           (word (make-word name #'push-parameter-as-cell :parameters (list address) :created-word? t)))
      (setf (word-inline-forms word) `((stack-push data-stack ,address))
            (word-inlineable? word) t)
      (add-and-register-word fs word address))))

(define-word subtract-double-single (:word "M-")
  "( d1 n - d2 )"
  "Subtract the single N from the double D1, producing the double result D2"
  (let ((n (cell-signed (stack-pop data-stack)))
        (d1 (stack-pop-double data-stack)))
    (stack-push-double data-stack (- d1 n))))

(define-word divide-double-single (:word "M/")
  "( d n1 - n2 )"
  "Divide the double D by the single N1, producing the single quotient N2"
  (let ((n1 (cell-signed (stack-pop data-stack)))
        (d (stack-pop-double data-stack)))
    (if (zerop n1)
        (forth-exception :divide-by-zero)
        (stack-push data-stack (cell-signed (truncate d n1))))))

(define-word not (:word "NOT")
  "( n - flag )"
  "Identical to 0=, used for program clarity to reverse the results of a previous test"
  (stack-push data-stack (if (zerop (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word simple-parse-number (:word "NUMBER" :inlineable? nil)
  "( c-addr u - n | d )"
  "Attempt to convert the string at C-ADDR of length U into an integer. If the string contains punctuation, return the"
  "double integer D. If the string does not contain punctuation, return the single integer N. If the conversion fails, ABORT"
  (let ((length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of string must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address length)
      (multiple-value-bind (type value)
          (interpret-number (forth-string-to-native forth-memory offset length) base :allow-floats? nil)
        (case type
          (:double
           (stack-push-double data-stack value))
          (:single
           (stack-push data-stack value))
          (otherwise
           (forth-exception :parse-integer-failure)))))))

(define-word maybe-parse-number (:word "NUMBER?" :inlineable? nil)
  "( c-addr u - 0 | n 1 | d 2 )"
  "Similar to NUMBER above but does not abort. If conversion fails, push 0 onto the top of the data stack."
  "If conversion suceeds, push the value and then push 1 if a single integer and 2 if a double integer."
  (let ((length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of string must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address length)
      (multiple-value-bind (type value)
          (interpret-number (forth-string-to-native forth-memory offset length) base :allow-floats? nil :signal-overflow? nil)
        (case type
          (:double
           (stack-push-double data-stack value)
           (stack-push data-stack 2))
          (:single
           (stack-push data-stack value)
           (stack-push data-stack 1))
          (otherwise
           (stack-push data-stack 0)))))))

(define-word create-word-list (:word "VOCABULARY")
  "VOCABULARY <name>"
  "Create an empty word list and define NAME to replace the first word list in the sarch order with this new list"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((dict (vocabulary word-lists name))
           (word (make-word name #'replace-top-of-search-order-with-parameter :parameters (list dict))))
      (add-and-register-word fs word))))


;;; Words defined by SwiftForth (https://www.forth.com/swiftforth/)

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
  (parse files #\} :multiline? t))

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
  (reset-memory memory)
  (register-predefined-words word-lists execution-tokens (data-space-high-water-mark memory)))

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


;;; Words defined by CL-Forth

(define-word break (:word "BREAK")
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

(define-word reload (:word "RELOAD")
  "RELOAD <name>"
  "Updates the definition of a predefined word"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((entry (gethash name *predefined-words*))
           (word-list (car entry))
           (new-word (cdr entry)))
      (when (null new-word)
        (forth-exception :undefined-word "~A is not a predefined word" name))
      (let ((old-word (lookup word-lists name)))
        (if old-word
            (setf (word-code old-word) (word-code new-word)
                  (word-smudge? old-word) (word-smudge? new-word)
                  (word-immediate? old-word) (word-immediate? new-word)
                  (word-compile-only? old-word) (word-compile-only? new-word)
                  (word-inlineable? old-word) (word-inlineable? new-word)
                  (word-inline-forms old-word) (word-inline-forms new-word))
            (add-word (word-list word-lists word-list) new-word :override t))))))

(define-word remove (:word "REMOVE")
  "REMOVE <name>"
  "Remove the current definition of NAME"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      (delete-word (word-parent word) execution-tokens word))))

(define-word inlineable (:word "INLINEABLE")
  "Make the most recent definition an inlineable word"
  (when definition
    (setf (word-inlineable? (definition-word definition)) t)))

(define-word not-interpreted (:word "NOTINTERPRETED")
  "Make the most recent definition only available when compiling a definition"
  (when definition
    (setf (word-compile-only? (definition-word definition)) t)))

(define-word show-float-stack (:word ".SF")
  "Show the contents of the floating-point stack"
  (show-stack float-stack base))

(define-word show-return-stack (:word ".SR")
  "Show the contents of the return stack"
  (show-stack return-stack base))

(define-word show-definition-code (:word "SHOW-BACKTRACES")
  "( - addr )"
  "Return the address of the flag that controls whether to show a backtrace when an exception occurs"
  (stack-push data-stack (state-slot-address memory 'show-backtraces-on-error?)))
