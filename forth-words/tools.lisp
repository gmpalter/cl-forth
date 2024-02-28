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

;;; DUMP
;;; WORDS
