(in-package #:forth)

;;; Paragraph numbers refer to the Forth Programmer's Handbook, 3rd Edition

;;; 2.1.4 Programmer Conveniences

(define-word dump-stack (:word ".S")
  "( - )"
  "Display the contents of the data stack in the current base"
  (let ((cells (stack-cells data-stack))
        (depth (stack-depth data-stack)))
    (if (zerop depth)
        (write-line "Data stack empty")
        (progn
          (write-line "Contents of data stack::")
          (dotimes (i depth)
            (format t "~2D: ~VR~%" i base (aref cells (- depth i 1))))))))

(define-word print-tos (:word "?")
  "( a-addr - )"
  "Display the contents of the memory address A-ADDR as a signed integer in the current base"
  (format t "~VR " base (cell-signed (memory-cell memory (stack-pop data-stack)))))

(define-word dump-memory (:word "DUMP" :inlineable? nil)
  "( a-addr +n - )"
  "Display the contents of N bytes at A-ADDR in data space"
  (let* ((count (stack-pop data-stack))
         (address (stack-pop data-stack))
         (end-address (+ address count)))
    (loop while (plusp count)
          ;; Addresses in MEMORY are 56 bits
          do (format t "~&~14,'0X: " address)
             (loop with byte-address = address
                   ;; Four "cells" at a time
                   for i from 0 below 4
                   while (< byte-address end-address)
                   do (loop with pseudo-cell = 0
                            with top = (min (1- count) 7)
                            for j downfrom top to 0
                            do ;; Memory is little-endian
                               (setf pseudo-cell (logior (ash pseudo-cell 8) (memory-byte memory (+ byte-address j))))
                               (decf count)
                            finally (format t "~V,'0X " (* 2 (- top j)) pseudo-cell))
                   (incf byte-address +cell-size+))
             (incf address (* 4 +cell-size+)))
    (fresh-line)))

(define-word show-words (:word "WORDS" :inlineable? nil)
  "List all the definition names in the first word list of the search order"
  (show-words (first (word-lists-search-order word-lists))))


;;; 6.4.2

;;; AHEAD
;;; CS-PICK
;;; CS-ROLL
