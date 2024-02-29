(in-package #:forth)

;;; Paragraph numbers refer to the Forth Programmer's Handbook, 3rd Edition

;;; 6.6.2 Managing Word Lists

(define-word duplicate-top (:word "ALSO")
  "Duplicate the first word list in the search order, increasing the size of the search order by 1."
  "Commonly used in the phrase \"ALSO <name>\" to add NAME to the top of the search order"
  (also word-lists))

(define-word change-compilation-word-list (:word "DEFINITIONS")
  "Change the compilation word list to be the same as the first word list in the search order"
  (definitions word-lists))

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
