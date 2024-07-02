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

;;; Locals words as defined in Section 13 of the Forth 2012 specification

(define-word define-local (:word "(LOCAL)" :immediate? t :compile-only? t)
  "Execution: ( c-addr u - )"
  "When executed during compilation, (LOCAL) passes a message to the system that has one of two meanings."
  "If U is non-zero, the message identifies a new local whose definition name is given by the string of characters"
  "identified by C-ADDR U. If U is zero, the message is \"last local\" and C-ADDR has no significance."
  "The result of executing (LOCAL) during compilation of a definition is to create a set of named local identifiers,"
  "each of which is a definition name, that only have execution semantics within the scope of that definition’s source."
  "LOCAL Execution: ( - x )"
  "Push the local's value, X, on the data stack. Each local is initialized by popping a value off the data stack and"
  "may be changed by preceeding the local's name with TO"
  "TO <local> Run-time: ( x - )"
  "Assign the value X to the local identifier <LOCAL>"
  (add-to-definition fs
    `(let ((count (cell-signed (stack-pop data-stack)))
           (address (stack-pop data-stack)))
       (cond ((plusp count)
              (multiple-value-bind (data offset)
                  (memory-decode-address memory address count)
                (let ((name (forth-string-to-native data offset count)))
                  (add-local-definition fs name))))
             ((zerop count)
              (end-local-definitions fs))
             ((minusp count)
              (forth-exception :invalid-numeric-argument "Size of (LOCAL) can't be negative"))))))


;;; Locals extension words as defined in Section 13 of the Forth 2012 specification

(define-word declare-locals-old (:word "LOCAL|" :immediate? t :compile-only? t)
  "LOCAL| name1 ... nameN |"
  "Create up to eight local identifiers by repeatedly skipping leading spaces, parsing name, and executing (LOCAL)."
  "The list of locals to be defined is terminated by | . Append the run-time semantics given below to the current definition."
  "Run-time: ( xn ... x2 x1 - )"
  "Initialize up to eight local identifiers, each of which takes as its initial value from the top stack item, removing it"
  "from the stack. Identifier NAME1 is initialized with X1, identifier NAME2 with X2, etc. When invoked, each local will"
  "return its value. The value of a local may be changed using TO"
  (let ((names nil))
    (loop do
      (let ((name (word files #\Space)))
        (when (null name)
          (forth-exception :zero-length-name "Missing |"))
        (when (string-equal name "|")
          (loop-finish))
        (when (= (length names) 8)
          (forth-exception :too-many-locals "LOCAL| only allows 8 names"))
        (push name names)))
    (dolist (name (reverse names))
      (add-local-definition fs name))
    (end-local-definitions fs)))

(define-word declare-locals-new (:word "{:" :immediate? t :compile-only? t)
  "{: ccc :}"
  "Compilation: Parse ccc according to the following syntax:"
  "    {: <arg>* [| ⟨val⟩*] [ –– ⟨out⟩*] :}"
  "where ⟨ARG⟩, ⟨VAL⟩ and ⟨OUT⟩ are local names, and I is the number of ⟨arg⟩ names given."
  "Append the run-time semantics below."
  "Run-time: ( x1 ... xn - )"
  "Create locals for ⟨ARG⟩s and ⟨VAL⟩s. ⟨OUT⟩s are ignored."
  "⟨ARG⟩ names are initialized from the data stack, with the top of the stack being assigned to the right most ⟨ARG⟩ name."
  "⟨VAL⟩ names are uninitialized."
  "⟨VAL⟩ and ⟨ARG⟩ names have the execution semantics given below."
  "NAME Execution: ( - x )"
  "Place the value currently assinged to NAME on the stack."
  "TO <name> Run-time: (x - )"
  "Set NAME to the value X"
  (let ((args? t)
        (args nil)
        (vals? nil)
        (vals nil))
    (loop do
      (let ((name (word files #\Space)))
        (when (null name)
          (forth-exception :zero-length-name "Missing :}"))
        (cond ((string-equal name "|")
               (unless args?
                 (forth-exception :invalid-local-name "\"|\" appears twice in {: ... :}"))
               (setf args? nil
                     vals? t))
              ((string-equal name "--")
               (unless (or args? vals?)
                 (forth-exception :invalid-local-name "\"--\" appears twice in {: ... :}"))
               (setf args? nil
                     vals? nil))
              ((string-equal name ":}")
               (loop-finish))
              (t
               (flet ((check-name ()
                        (when (or (and (= (length name) 1) (not (alpha-char-p (aref name 0))))
                                  (member (aref name (1- (length name))) '(#\: #\[ #\^) :test #'char-equal))
                          (forth-exception :invalid-local-name "\"~A\" is not a valid LOCAL name" name))))
                 (cond (args?
                        (check-name)
                        (push name args))
                       (vals?
                        (check-name)
                        (push name vals))))))))
    (dolist (name args)
      (add-local-definition fs name t))
    (dolist (name vals)
      (add-local-definition fs name nil))
    (end-local-definitions fs)))
