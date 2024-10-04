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

;;; Search-Order words as defined in Section 16 of the Forth 2012 specification

(define-word change-compilation-word-list (:word "DEFINITIONS")
  "Change the compilation word list to be the same as the first word list in the search order"
  (definitions word-lists))

(define-word find (:word "FIND")
  "( c-addr -- c-addr 0 | xt 1 | xt -1)"
  "Find the definition named in the counted string at C-ADDR. If the definition is not found after searching all"
  "the word lists in the search order, return C-ADDR and zero."
  "If the definition is found, return its execution token XT. If the definition is immediate, also return one (1),"
  "otherwise also return minus-one (-1)"
  (let ((string (stack-pop data-stack)))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory string (1+ +longest-counted-string+))
      (let ((word (lookup word-lists (forth-counted-string-to-native forth-memory offset))))
        (cond (word
               (stack-push data-stack (xt-token (word-execution-token word)))
               (stack-push data-stack (if (word-immediate? word) 1 -1)))
              (t
               (stack-push data-stack string)
               (stack-push data-stack 0)))))))

(define-word forth-wordlist (:word "FORTH-WORDLIST")
  "( -- wid)"
  "Return WID, the identifier of the word list that includes all standard words provided by the implementation."
  "This word list is initially the compilation word list and is part of the initial search order"
  (stack-push data-stack (dictionary-wid (word-lists-forth-word-list word-lists))))

(define-word get-current (:word "GET-CURRENT")
  "( -- wid)"
  "Return WID, the identifier of the compilation word list"
  (stack-push data-stack (dictionary-wid (word-lists-compilation-word-list word-lists))))

(define-word get-order (:word "GET-ORDER")
  "( -- widn ... wid1 n )"
  "Returns the number of word lists N in the search order and the word list identifiers WIDn . . . WID1 identifying these"
  "word lists. WID1 identifies the word list that is searched first, and WIDn the word list that is searched last"
  (let ((wids nil))
    (dolist (wl (word-lists-search-order word-lists))
      (push (dictionary-wid wl) wids))
    (dolist (wid wids)
      (stack-push data-stack wid))
    (stack-push data-stack (length wids))))
  
(define-word search-wordlist (:word "SEARCH-WORDLIST")
  "( c-addr u wid -- 0 | xt 1 | xt -1 )"
  "Find the definition identified by the string C-ADDR U in the word list identified by WID."
  "If the definition is not found, return zero. If the definition is found, return its execution token XT"
  "and one (1) if the definition is immediate, minus-one (-1) otherwise"
  (let ((wl (lookup-wid word-lists (stack-pop data-stack)))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Word name length must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address count)
      (let* ((name (forth-string-to-native forth-memory offset count))
             (word (search-dictionary wl name)))
        (cond (word
               (stack-push data-stack (xt-token (word-execution-token word)))
               (stack-push data-stack (if (word-immediate? word) 1 -1)))
              (t
               (stack-push data-stack 0)))))))

(define-word set-current (:word "SET-CURRENT")
  "( wid -- )"
  "Set the compilation word list to the word list identified by WID"
  (let ((wl (lookup-wid word-lists (stack-pop data-stack))))
    (setf (word-lists-compilation-word-list word-lists) wl)))

(define-word set-order (:word "SET-ORDER")
  "( widn ... wid1 n -- )"
  "Set the search order to the word lists identified by WIDn . . . WID1."
  "Subsequently, word list WID1 will be searched first, and word list WIDn searched last."
  "If N is zero, empty the search order. If N is minus one, set the search order to the"
  "implementation-defined minimum search order"
  (let ((n (cell-signed (stack-pop data-stack))))
    (cond ((plusp n)
           (let ((new-search-order nil))
             (dotimes (i n)
               (push (lookup-wid word-lists (stack-pop data-stack)) new-search-order))
             (setf (word-lists-search-order word-lists) (reverse new-search-order))))
          ((zerop n)
           (setf (word-lists-search-order word-lists) nil))
          ((= n -1)
           (only word-lists))
          (t
           (forth-exception :invalid-numeric-argument)))))

(define-word new-wordlist (:word "WORDLIST")
  "( -- wid )"
  "Create a new empty word list, returning its word list identifier WID"
  (let ((wl (word-list word-lists nil :if-not-found :create)))
    (stack-push data-stack (dictionary-wid wl))))


;;; Search-Order extension words as defined in Section 16 of the Forth 2012 specification

(define-word duplicate-top (:word "ALSO")
  "Duplicate the first word list in the search order, increasing the size of the search order by 1."
  "Commonly used in the phrase \"ALSO <name>\" to add NAME to the top of the search order"
  (also word-lists))

(define-word forth-on-top (:word "FORTH")
  "Replace the first the word list in the search order with the FORTH word list"
  (replace-top-of-search-order word-lists (word-lists-forth-word-list word-lists)))

(define-word reset-search-order (:word "ONLY")
  "Reduce the search order to the minimum set of word lists. (I.e., just FORTH)"
  (only word-lists))

(define-word display-search-order (:word "ORDER" :inlineable? nil)
  "Display the name of all word lists in the search order as well as the name of the compilation word list"
  (format t "~& Context: ")
  (dolist (dict (word-lists-search-order word-lists))
    (format t "~A " (dictionary-name dict)))
  (format t "~& Current: ~A~%" (dictionary-name (word-lists-compilation-word-list word-lists))))

(define-word pop-search-order (:word "PREVIOUS")
  "Remove the first entry from the search order"
  (previous word-lists))
