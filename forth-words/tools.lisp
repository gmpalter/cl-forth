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

;;;---*** SEE

(define-word show-words (:word "WORDS" :inlineable? nil)
  "List all the definition names in the first word list of the search order"
  (show-words (first (word-lists-search-order word-lists))))


;;; 6.1.5 Text Interpreter Conditionals

(define-word defined (:word "[DEFINED]" :immediate? t)
  "[DEFINED] <name>" "( - flag )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Return a true flag if name is the name of a word"
  "that can be found (according to the rules in the system’s FIND); otherwise return a false flag"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (if word
          (stack-push data-stack +true+)
          (stack-push data-stack +false+)))))

(define-word undefined (:word "[UNDEFINED]" :immediate? t)
  "[UNDEFINED] <name>" "( - flag )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Return a false flag if NAME is the name of a word"
  "that can be found (according to the rules in the system’s FIND); otherwise return a true flag"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (if word
          (stack-push data-stack +false+)
          (stack-push data-stack +true+)))))

(define-word interpreted-if (:word "[IF]" :immediate? t)
  "( flag - )"
  "If FLAG is true, do nothing. Otherwise, skipping leading spaces, parse and discard space-delimited words"
  "from the parse area, including nested occurrences of [IF]... [THEN] and [IF] ... [ELSE] ... [THEN], until either the"
  "word [ELSE] or the word [THEN] has been parsed and discarded. If the parse area becomes exhausted, it is refilled"
  "as with REFILL"
  (block interpreted-if
    (when (falsep (stack-pop data-stack))
      (do ((nesting 0)
           (word (word files #\Space) (word files #\Space)))
          ()
        (cond ((null word)
               (unless (refill files)
                 (forth-exception :if/then/else-exception)))
              ((string-equal word "[IF]")
               (incf nesting))
              ((string-equal word "[ELSE]")
               (when (zerop nesting)
                 (return-from interpreted-if (values))))
              ((string-equal word "[THEN]")
               (if (zerop nesting)
                   (return-from interpreted-if (values))
                   (decf nesting))))))))

(define-word interpreted-else (:word "[ELSE]" :immediate? t)
  "Skipping leading spaces, parse and discard space-delimited words from the parse area, including nested occurrences"
  "of [IF] ... [THEN] and [IF] ... [ELSE] ... [THEN], until the word [THEN] has been parsed and discarded."
  "If the parse area be- comes exhausted, it is refilled as with REFILL"
  (block interpreted-else
    (do ((nesting 0)
         (word (word files #\Space) (word files #\Space)))
        ()
      (cond ((null word)
             (unless (refill files)
               (forth-exception :if/then/else-exception)))
            ((string-equal word "[IF]")
             (incf nesting))
            ((string-equal word "[ELSE]")
             (when (zerop nesting)
               (return-from interpreted-else (values))))
            ((string-equal word "[THEN]")
             (if (zerop nesting)
                 (return-from interpreted-else (values))
                 (decf nesting)))))))

(define-word interpreted-then (:word "[THEN]" :immediate? t)
  "Does nothing"
  )


;;; 6.4.2 The Control-flow Stack and Custom Compiling Structures

(define-word ahead (:word "AHEAD" :immediate? t :compile-only? t)
  "(C: — orig )"
  "At compile time, begin an unconditional forward branch by placing ORIG (the location of the unresolved branch)"
  "on the control-flow stack. The behavior is incomplete until the ORIG is resolved, e.g., by THEN."
  "At run time, resume execution at the location provided by the resolution of this ORIG"
  (let ((branch (make-branch-reference :ahead)))
    (stack-push control-flow-stack branch)
    (execute-branch fs branch)))

(define-word cs-pick (:word "CS-PICK")
  "(S: u - ) (C: xu ... x0 - xu ... x0 xu ) "
  "Place a copy of the uth control-stack entry on the top of the control stack. The zeroth item is on top of the"
  "control stack; i.e., 0 CS-PICK is equivalent to DUP and 1 CS-PICK is equivalent to OVER."
  (stack-pick control-flow-stack (cell-unsigned (stack-pop data-stack))))

(define-word cs-roll (:word "CS-ROLL")
  "(S: u - ) (C: x(u-1) xu x(u+1) ... x0 - x(u-1) x(u+1) ... x0 xu )"
  "Move the Uth control-stack entry to the top of the stack, pushing down all the control-stack entries in between."
  "The zeroth item is on top of the stack; i.e., 0 CS-ROLL does nothing, 1 CS-ROLL is equivalent to SWAP, and"
  "2 CS-ROLL is equivalent to ROT"
  (stack-roll control-flow-stack (cell-unsigned (stack-pop data-stack))))

