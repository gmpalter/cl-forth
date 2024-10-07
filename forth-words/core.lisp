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

;;; Core words as defined in Section 6 of the Forth 2012 specification

(define-word write-cell (:word "!")
  "( x a-addr -- )"
  "Store the cell X at the address A-ADDR"
  (setf (memory-cell memory (stack-pop data-stack)) (stack-pop data-stack)))

(define-word add-digit-to-picture-buffer (:word "#")
  "( ud1 -- ud2 )"
  "Divide UD1 by the number in BASE giving the quotient UD2 and the remainder N. (N is the least significant digit of UD1.)"
  "Convert N to external form and add the resulting character to the beginning of the pictured numeric output string"
  (unless (pictured-buffer-active? (memory-pictured-buffer memory))
    (forth-exception :no-pictured-output "Can't use # outside <# ... #>"))
  (flush-optimizer-stack)
  (let ((ud1 (stack-pop-double-unsigned data-stack)))
    (multiple-value-bind (ud2 digit)
        (floor ud1 base)
      (add-to-pictured-buffer (memory-pictured-buffer memory) (forth-char (digit-char digit base)))
      (stack-push-double data-stack ud2))))

(define-word finish-pictured (:word "#>")
  "( xd -- c-addr u)"
  "Drop XD. Make the pictured numeric output string available as a character string."
  "C-ADDR and U specify the resulting character string"
  (unless (pictured-buffer-active? (memory-pictured-buffer memory))
    (forth-exception :no-pictured-output "#> without matching <#"))
  (flush-optimizer-stack)
  (stack-pop-double data-stack)
  (multiple-value-bind (c-addr u)
      (finish-pictured-buffer (memory-pictured-buffer memory))
    (stack-push data-stack c-addr)
    (stack-push data-stack u)))

(define-word add-digits-to-picture-buffer (:word "#S")
  "( ud1 -- ud2 )"
  "Convert one digit of UD1 according to the rule for #. Continue conversion until the quotient is zero. UD2 is zero"
  (unless (pictured-buffer-active? (memory-pictured-buffer memory))
    (forth-exception :no-pictured-output "Can't use #S outside <# ... #>"))
  (flush-optimizer-stack)
  (let ((ud1 (stack-pop-double-unsigned data-stack)))
    (loop do (multiple-value-bind (ud2 digit)
                 (floor ud1 base)
               (add-to-pictured-buffer (memory-pictured-buffer memory) (forth-char (digit-char digit base)))
               (setf ud1 ud2))
          until (zerop ud1))
    (stack-push-double data-stack 0)))

(define-word lookup-xt (:word "'")
  "' <name>" "( -- xt )"
  "Lookup NAME and return its execution token or abort if not found"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      (stack-push data-stack (xt-token (word-execution-token word))))))

;;; ( extended by the File-Access word set

(define-word multiply (:word "*")
  "( n1 n2 -- n3 )"
  (stack-push data-stack (cell-signed (* (stack-pop data-stack) (stack-pop data-stack)))))

(define-word multiply-divide (:word "*/")
  "( n1 n2 n3 -- n4 )"
  (let ((n3 (cell-signed (stack-pop data-stack)))
        (n2 (cell-signed (stack-pop data-stack)))
        (n1 (cell-signed (stack-pop data-stack))))
    (stack-push data-stack (cell-signed (truncate (* n1 n2) n3)))))

(define-word multiply-divide-mod (:word "*/MOD")
  "( n1 n2 n3 -- n4 n5 )"
  (let ((n3 (cell-signed (stack-pop data-stack)))
        (n2 (cell-signed (stack-pop data-stack)))
        (n1 (cell-signed (stack-pop data-stack))))
    (if (zerop n3)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate (* n1 n2) n3)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word add (:word "+")
  "( n1 n2 -- n3 )"
  (stack-push data-stack (cell-signed (+ (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))))))

(define-word incf-cell (:word "+!")
  "( n a-addr -- )"
  "Add N to the contents of the cell at A-ADDR and store the result back into A-ADDR"
  (let ((address (stack-pop data-stack))
        (n (stack-pop data-stack)))
    (setf (memory-cell memory address) (cell-signed (+ (memory-cell memory address) n)))))

(define-word loop (:word "+LOOP" :immediate? t :compile-only? t)
  "( n -- )"
  "Like LOOP, but increment the index by the specified signed value n. After incrementing, if the index crossed the"
  "boundary between the loop limit minus one and the loop limit, the loop is terminated as with LOOP."
  (verify-control-structure fs :do 2)
  (let ((again (stack-cell control-flow-stack 0))
        (done (stack-cell control-flow-stack 1)))
    (add-to-definition fs
      '(flush-optimizer-stack))
    (execute-branch-when fs again
      (let* ((increment (cell-signed (stack-pop data-stack)))
             (limit (cell-signed (stack-cell loop-stack 1)))
             (before (cell-signed (stack-cell loop-stack 0)))
             (after (+ before increment))
             (after-signed (cell-signed after)))
        (setf (stack-cell loop-stack 0) after)
        (cond ((plusp increment)
               (if (= after after-signed)
                   (not (and (< before limit) (>= after limit)))
                   ;; Value wrapped around .. check unsigned
                   (let ((limit-unsigned (cell-unsigned limit))
                         (before-unsigned (cell-unsigned before))
                         (after-unsigned (cell-unsigned after)))
                     (not (and (< before-unsigned limit-unsigned) (>= after-unsigned limit-unsigned))))))
              ((minusp increment)
               (if (= after after-signed)
                   (not (and (>= before limit) (< after limit)))
                   ;; Value wrapped around .. check unsigned
                   (let ((limit-unsigned (cell-unsigned limit))
                         (before-unsigned (cell-unsigned before))
                         (after-unsigned (cell-unsigned after)))
                     (not (and (>= before-unsigned limit-unsigned) (< after-unsigned limit-unsigned))))))
              (t t))))
    (add-to-definition fs
      `(stack-pop loop-stack)
      `(stack-pop loop-stack))
    (stack-pop control-flow-stack)
    (stack-pop control-flow-stack)
    (resolve-branch fs done)))

(define-word create-cell (:word ",")
  "( x -- )"
  "Allocate one cell in data space and store x in the cell"
  (flush-optimizer-stack :contains (data-space-high-water-mark memory))
  (setf (memory-cell memory (allocate-memory memory +cell-size+)) (stack-pop data-stack)))

(define-word subtract (:word "-")
  "( n1 n2 -- n3 )"
  ;; As the first value popped off the stack is N2, we'll compute (- (- n2 n1)) which is equivalent to (- n1 n2).
  (stack-push data-stack (cell-signed (- (- (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack)))))))

(define-word print-tos (:word ".")
  "( n -- )"
  "Display the top cell of the data stack as a signed integer in the current base"
  (format t "~VR " base (cell-signed (stack-pop data-stack))))

(define-word type-string (:word ".\"" :immediate? t :inlineable? nil)
  ".\" <text>\""
  "Type TEXT on the console."
  (let ((text (parse files #\")))
    (case (state fs)
      (:interpreting
       (write-string text))
      (:compiling
       (add-to-definition fs
         `(write-string ,text))))))

(define-word divide (:word "/")
  "( n1 n2 -- n3 )"
  (let ((n2 (cell-signed (stack-pop data-stack)))
        (n1 (cell-signed (stack-pop data-stack))))
    (if (zerop n2)
        (forth-exception :divide-by-zero)
        (stack-push data-stack (cell-signed (truncate n1 n2))))))

(define-word divide-mod (:word "/MOD")
  "( n1 n2 -- n3 n4 )"
  (let ((n2 (cell-signed (stack-pop data-stack)))
        (n1 (cell-signed (stack-pop data-stack))))
    (if (zerop n2)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate n1 n2)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word minusp (:word "0<")
  " ( n -- flag )"
  "Return true if N is less than zero"
  (stack-push data-stack (if (minusp (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word zerop (:word "0=")
  " ( n -- flag )"
  "Return true if N is equal to zero"
  (stack-push data-stack (if (zerop (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word add-one (:word "1+")
  "( n1 -- n2 )"
  (stack-push data-stack (cell-signed (1+ (cell-signed (stack-pop data-stack))))))

(define-word subtract-one (:word "1-")
  "( n1 -- n2 )"
  (stack-push data-stack (cell-signed (1- (cell-signed (stack-pop data-stack))))))

(define-word write-two-cells (:word "2!")
  "( x1 x2 a-addr -- )"
  "Store the two cells X1 and X2 in the two cells beginning at the address A-ADDR"
  (let ((address (stack-pop data-stack))
        (data (stack-pop-double data-stack)))
    (setf (memory-double-cell memory address) data)))

(define-word ash-left-1 (:word "2*")
  "( x1 -- x2 )"
  (stack-push data-stack (ash (cell-signed (stack-pop data-stack)) 1)))

(define-word ash-right-1 (:word "2/")
  "( x1 -- x2 )"
  (stack-push data-stack (ash (cell-signed (stack-pop data-stack)) -1)))

(define-word read-two-cells (:word "2@")
  "( a-addr -- x1 x2 )"
  "Push the contents of the two cells starting at the address A-ADDR onto the data stack"
  (stack-push-double data-stack (memory-double-cell memory (stack-pop data-stack))))

(define-word stack-2drop (:word "2DROP")
  "( x1 x2 -- )"
  "Remove the top pair of cells from the data stack"
  (stack-2drop data-stack))

(define-word stack-2dup (:word "2DUP")
  "( x1 x2 -- x1 x2 x1 x2 )"
  "Duplicate the top cell pair on the data stack"
  (stack-2dup data-stack))

(define-word stack-2over (:word "2OVER")
  "( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )"
  "Copy cell pair X1 X2 to the top of the data stack"
  (stack-2over data-stack))

(define-word stack-2swap (:word "2SWAP")
  "( x1 x2 x3 x4 -- x3 x4 x1 x2 )"
  "Exchange the top two cell pairs on the data stack"
  (stack-2swap data-stack))

(define-word start-definition (:word ":")
  ": <name>"
  "Create a definition for <name>. Enter compilation state and start compiling the definition."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (begin-compilation fs name)))

(define-word finish-definition (:word ";" :immediate? t :compile-only? t)
  "Complete the current definition and make it available for use. Align the data space pointer to a cell boundary"
  (finish-compilation fs)
  (align-memory memory))

(define-word less-than (:word "<")
  " ( n1 n2 -- flag )"
  "Return true if N1 is less than N2"
  ;; As the first value popped off the stack is N2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (> (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word start-pictured (:word "<#")
  "Initialize the pictured numeric output conversion process"
  (flush-optimizer-stack)
  (start-pictured-buffer (memory-pictured-buffer memory)))

(define-word equal (:word "=")
  " ( n1 n2 -- flag )"
  "Return true if N1 is equal to N2"
  (stack-push data-stack (if (= (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word greater-than (:word ">")
  " ( n1 n2 -- flag )"
  "Return true if N1 is greater than N2"
  ;; As the first value popped off the stack is N2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (< (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word >body (:word ">BODY")
  "( xt -- a-addr )"
  "Return A-ADDR which is the data-field address corresponding to XT"
  (stack-push data-stack (find-body execution-tokens (stack-pop data-stack))))

(define-state-word >in :word ">IN")

(define-word parse-number (:word ">NUMBER" :inlineable? nil)
  "( ud1 c-addr u1 -- ud2 c-addr u2 )"
  "UD2 is the unsigned result of converting the characters within the string specified by C-ADDR1 U1 into digits,"
  "using the number in BASE, and adding each into UD1 after multiplying UD1 by the number in BASE. Conversion continues"
  "left-to-right until a character that is not convertible, including any '+' or '-', is encountered or the string is"
  "entirely converted. C-ADDR2 is the location of the first unconverted character or the first character past the end"
  "of the string if the string was entirely converted. U2 is the number of unconverted characters in the string."
  (let ((length (cell-unsigned (stack-pop data-stack)))
        (address (stack-pop data-stack))
        (value (stack-pop-double-unsigned data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of string must be positive"))
    (loop while (plusp length)
          for char = (native-char (memory-char memory address))
          for digit = (digit-char-p char base)
          if digit
            do (setf value (+ (* base value) digit))
               (incf address +char-size+)
               (decf length +char-size+)
          else
            do (loop-finish)
          finally
             (stack-push-double data-stack value)
             (stack-push data-stack address)
             (stack-push data-stack length))))

(define-word move-to-return-stack (:word ">R")
  "(S: x -- ) (R: -- x )"
  "Pop the top item off the data stack and push it onto the return stack"
  (stack-push return-stack (stack-pop data-stack)))

(define-word stack-?dup (:word "?DUP")
  "( x -- 0 | x x )"
  "Conditionally duplicate the top of the data stack if it is non-zero"
  (stack-?dup data-stack))

(define-word read-cell (:word "@")
  "( a-addr -- x )"
  "Push the contents of the cell at the address A-ADDR onto the data stack"
  (stack-push data-stack (cell-signed (memory-cell memory (stack-pop data-stack)))))

;;; ABORT extended by the Exception word set

;;; ABORT" extended by the Exception word set

(define-word abs (:word "ABS")
  "( n1 -- n2 )"
  "Push the absolute value of N1 onto the data stack"
  (stack-push data-stack (cell-signed (abs (stack-pop data-stack)))))

(define-word accept (:word "ACCEPT")
  "( c-addr +n1 -- +n2 )"
  "Receive a string of at most +N1 characters. Input terminates when an implementation-defined line terminator is received."
  "When input terminates, nothing is appended to the string. +N2 is the length of the string stored at C-ADDR"
  (let ((count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "ACCEPT buffer size must be positive"))
    ;; NOTE: In order to comply with the Forth standard, we have to read one character at a time
    ;;       until we either get a Newline or fill the buffer. (Sigh)
    (let ((nread 0))
      (declare (type fixnum nread))
      (loop for i fixnum below count
            for char = (read-char nil nil :eof)
            if (or (eq char :eof) (eql (the character char) #\Newline))
              do (loop-finish)
            else
              do (setf (memory-char memory (the fixnum (+ address i))) (forth-char (the character char))
                       nread (the fixnum (1+ nread))))
      (stack-push data-stack nread))))

(define-word align (:word "ALIGN")
  "( -- )"
  "If the data space pointer is not aligned, reserve enough space to align it"
  (align-memory memory))

(define-word aligned (:word "ALIGNED")
  "( addr -- a-addr) "
  "Return A-ADDR, the first aligned address greater than or equal to ADDR"
  (let ((addr (stack-pop data-stack)))
    (stack-push data-stack
                (if (zerop (mod addr +cell-size+))
                    addr
                    (+ addr (- +cell-size+ (mod addr +cell-size+)))))))

(define-word allocate (:word "ALLOT")
  "( n -- )"
  "If N is greater than zero, reserve N address units of data space. If N is less than zero, release abs(N) address units"
  "of data space. If N is zero, leave the data-space pointer unchanged."
  (let ((count (cell-signed (stack-pop data-stack))))
    (flush-optimizer-stack :contains (data-space-high-water-mark memory))
    (cond ((plusp count)
           (allocate-memory memory count))
          ((minusp count)
           (deallocate-memory memory (abs count)))
          ((zerop count)
           nil))))

(define-word and (:word "AND")
  "( x1 x2 -- x3 )"
  "Return the bitwise logical and of X1 with X2"
  (stack-push data-stack (cell-unsigned (logand (stack-pop data-stack) (stack-pop data-stack)))))

(define-state-word base :word "BASE")

(define-word begin (:word "BEGIN" :immediate? t :compile-only? t)
  "Mark the destination of a backward branch for use by the other indefinite structure words AGAIN, UNTIL or REPEAT"
  (let ((branch (make-branch-reference :begin)))
    (stack-push control-flow-stack branch)
    (resolve-branch fs branch)))

(define-word blank (:word "BL")
  "( -- char )"
  "Push the character for a blank or space onto the data stack"
  (stack-push data-stack +forth-char-space+))

(define-word write-char (:word "C!")
  "( char a-addr -- )"
  "Store the character CHAR at the address A-ADDR"
  (let ((address (stack-pop data-stack))
        (char (extract-char (stack-pop data-stack))))
    (setf (memory-char memory address) char)))

(define-word create-char (:word "C,")
  "( char -- )"
  "Allocate space for one character in data space and store CHAR"
  (flush-optimizer-stack :contains (data-space-high-water-mark memory))
  (setf (memory-char memory (allocate-memory memory +char-size+)) (extract-char (stack-pop data-stack))))

(define-word read-char (:word "C@")
  "( a-addr -- char )"
  "Push the character at the address A-ADDR onto the data stack"
  (stack-push data-stack (memory-char memory (stack-pop data-stack))))

(define-word cell-incf (:word "CELL+")
  "( a-addr1 -- a-addr2 )"
  "Add the size of a cell in bytes to A-ADDR1, giving A-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +cell-size+)))

(define-word cells-size (:word "CELLS")
  "( n1 -- n2 )"
  "Return the size in bytes of n1 cells"
  (stack-push data-stack (* (stack-pop data-stack) +cell-size+)))

(define-word char (:word "CHAR")
  "CHAR <c>" "( -- char )"
  "Parse the next word, usually a single character, and push the ASCII value of its first character onto the data stack"
  (let ((char (word files #\Space)))
    (when (null char)
      (forth-exception :zero-length-name))
    (stack-push data-stack (forth-char (aref char 0)))))

(define-word char-incf (:word "CHAR+")
  "( a-addr1 -- a-addr2 )"
  "Add the size of a character in bytes to A-ADDR1, giving A-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +char-size+)))

(define-word chars-size (:word "CHARS")
  "( n1 -- n2 )"
  "Return the size in bytes of n1 characters"
  (stack-push data-stack (* (stack-pop data-stack) +char-size+)))

(define-word constant (:word "CONSTANT")
  "CONSTANT <name>" "( x -- )"
  "Create a dictionary entry for <name> which pushes signed integer X on the data stack"
  (let ((name (word files #\Space))
        (value (stack-pop data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'push-parameter-as-cell :parameters (make-parameters value))))
      (setf (word-inline-forms word) `((stack-push data-stack ,value))
            (word-inlineable? word) t)
      (add-and-register-word fs word (data-space-high-water-mark memory)))))

(define-word decode-counted-string (:word "COUNT")
  "( a-addr1 -- a-addr2 u )"
  "Return the size U of the counted string at A-ADDR1 and the address of its text"
  (let ((address (stack-pop data-stack)))
    ;; Length of a counted string is always a single byte regardless of character size
    (stack-push data-stack (1+ address))
    (stack-push data-stack (memory-byte memory address))))

(define-word terpri (:word "CR")
  "Cause subsequent output to the terminal to appear on a new line"
  (terpri))

(define-word create (:word "CREATE")
  "CREATE <name>"
  "Create a dictionary entry for <name> that returns the address of the next available location in data space"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (data-space-high-water-mark memory))
           (word (make-word name #'push-parameter-as-cell :parameters (make-parameters address) :created-word? t)))
      (setf (word-inline-forms word) `((stack-push data-stack ,address))
            (word-inlineable? word) t)
      (add-and-register-word fs word address))))

(define-word decimal (:word "DECIMAL")
  "Change the system base to decimal"
  (flush-optimizer-stack :contains (state-slot-address memory 'base))
  (setf base 10.))

(define-word stack-depth (:word "DEPTH")
  "( -- +n )"
  "Pushes the current depth of the data stack onto the data stack"
  (stack-push data-stack (stack-depth data-stack)))

(define-word do (:word "DO" :immediate? t :compile-only? t)
  "( n1 n2 -- )"
  "Establish the loop parameters. This word expects the initial loop index N2 on top of the stack, with the limit value N1"
  "beneath it. These values are removed from the stack and stored on the return stack when DO is executed"
  (let ((again (make-branch-reference :do))
        (done (make-branch-reference :do)))
    (stack-push control-flow-stack done)
    (stack-push control-flow-stack again)
    (add-to-definition fs
      `(let ((n2 (cell-signed (stack-pop data-stack)))
             (n1 (cell-signed (stack-pop data-stack))))
         (stack-push loop-stack n1)
         (stack-push loop-stack n2)))
    (resolve-branch fs again)))

(define-word does> (:word "DOES>" :immediate? t :compile-only? t)
  "Begin run-time behavior, specified in high-level Forth. At run time, the address of the parameter field"
  "of the instance of the defining word is pushed onto the stack before the run-time words are executed."
  "NOTE: The above description from the Programmer's Handbook is a bit vague"
  (compile-does> fs))

(define-word stack-drop (:word "DROP")
  "( x -- )"
  "Remove the top entry from the data stack"
  (stack-drop data-stack))

(define-word stack-dup (:word "DUP")
  "( x -- x x )"
  "Duplicate the top of the data stack"
  (stack-dup data-stack))

(define-word else (:word "ELSE" :immediate? t :compile-only? t)
  "Mark the end of the true part of a conditional structure, and commence the false part. May be ommitted if there"
  "are no words to be executed in the false case"
  (verify-control-structure fs :if)
  (let ((branch (make-branch-reference :if)))
    (execute-branch fs branch)
    (stack-push control-flow-stack branch))
  (stack-swap control-flow-stack)
  (let ((branch (stack-pop control-flow-stack)))
    (resolve-branch fs branch)))

(define-word write-char (:word "EMIT")
  " ( char -- )"
  "Type the character CHAR on the terminal"
  (write-char (native-char (extract-char (stack-pop data-stack)))))

;;; ENVIRONMENT? is defined in forth-words/environment.lisp

(define-word evaluate-string (:word "EVALUATE")
  "( i*x c-addr u -- j*x )"
  "Save the current input source specification. Store minus-one (-1) in SOURCE-ID. Make the string described by c-ADDR and U"
  "both the input source and input buffer, set >IN to zero, and interpret. When the parse area is empty, restore the prior"
  "input source specification. Other stack effects are due to the words EVALUATEd."
  (flush-optimizer-stack)
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to EVALUATE can't be negative"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address count)
      (let ((string (forth-string-to-native forth-memory offset count)))
        (source-push files :evaluate string :source-address address)))
    (interpreter/compiler fs :toplevel? nil)))

;;; Declared IMMEDIATE and not inlineable so we can generate the proper PC for the call
(define-word execute (:word "EXECUTE" :immediate? t :inlineable? nil)
  "( i*x xt -- j*x )"
  "Remove XT from the stack and perform the semantics identified by it. Other stack effects are due"
  "to the worde executed"
  (case (state fs)
    (:interpreting
     (execute execution-tokens (stack-pop data-stack) fs))
    (:compiling
     (add-to-definition fs
       `(flush-optimizer-stack)
       `(execute execution-tokens (stack-pop data-stack) fs ,(next-psuedo-pc definition))))))

;;; Marked as IMMEDIATE so we can generate the proper code sequence
(define-word exit (:word "EXIT" :immediate? t)
  "Return control immediately to the calling definition. Before executing EXIT, a program must remove any items"
  "explicitly stored on the return stack. If EXIT is called within a DO ... LOOP, UNLOOP must be executed first"
  "to discard the loop-control parameters"
  (execute-branch fs (definition-exit-branch definition)))

(define-word fill-memory (:word "FILL")
  "( c-addr u b -- )"
  "Set a region of memory, at address C-ADDR and of length U, to the byte B"
  (let ((byte (ldb (byte 8 0) (stack-pop data-stack)))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to FILL can't be negative"))
    (memory-fill memory address count byte)))

;;; FIND extended by the Search-Order word set

(define-word floor-mod (:word "FM/MOD")
  "( d n1 -- n2 n3 )"
  "Divide the double precision integer D by the integer N1 using floored division"
  "Push the remainder N2 and quotient N3 onto the data stack"
  (let ((n1 (cell-signed (stack-pop data-stack)))
        (d (stack-pop-double data-stack)))
    (if (zerop n1)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (floor d n1)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word here (:word "HERE")
  "( -- a-addr )"
  "Push the address of the next available memory location in data space onto the stack"
  (stack-push data-stack (data-space-high-water-mark memory)))

(define-word add-char-to-picture-buffer (:word "HOLD")
  "( char -- )"
  "Add char to the beginning of the pictured numeric output string"
  (unless (pictured-buffer-active? (memory-pictured-buffer memory))
    (forth-exception :no-pictured-output "Can't use HOLD outside <# ... #>"))
  (add-to-pictured-buffer (memory-pictured-buffer memory) (stack-pop data-stack)))

(define-word index1 (:word "I" :immediate? t :compile-only? t)
  "( -- n )"
  "Push a copy of the current value of the index onto the data stack"
  (add-to-definition fs
    `(when (< (stack-depth loop-stack) 2)
       (forth-exception :no-loop-parameters "I not inside DO loop"))
    `(stack-push data-stack (stack-cell loop-stack 0))
    `(flush-optimizer-stack :count 1)))

(define-word if (:word "IF" :immediate? t :compile-only? t)
  "( flag -- )"
  "If FLAG is zero, branch to the code immediately following an ELSE if one is present; if ELSE is ommitted, branch"
  "to the point following THEN. If FLAG is true, continue execution with the code immediately following the IF and"
  "branch over any code following an ELSE to the point following THEN"
  (let ((branch (make-branch-reference :if)))
    (stack-push control-flow-stack branch)
    (execute-branch-when fs branch
      (falsep (cell-unsigned (stack-pop data-stack))))))

(define-word immediate (:word "IMMEDIATE")
  "Make the most recent definition an immediate word"
  (when definition
    (setf (word-immediate? (definition-word definition)) t)))

(define-word invert (:word "INVERT")
  "( x1 -- x2 )"
  "Invert all bits of X1, giving the logical inverse X2"
  (stack-push data-stack (cell-unsigned (lognot (stack-pop data-stack)))))

(define-word index2 (:word "J" :immediate? t :compile-only? t)
  "( -- n )"
  "Push a copy of the next-outer loop index onto the data stack. When two DO ... LOOPs are nested, this obtains"
  "the value of the outer index from inside the inner loop."
  (add-to-definition fs
    `(when (< (stack-depth loop-stack) 4)
       (forth-exception :no-loop-parameters "J not inside DO ... DO ... LOOP ... LOOP"))
    `(stack-push data-stack (stack-cell loop-stack 2))
    `(flush-optimizer-stack :count 1)))

;;;---*** KEY

(define-word leave (:word "LEAVE" :immediate? t :compile-only? t)
  "Discard loop parameters and continue execution immediately following the next LOOP or +LOOP containing this LEAVE"
  (let ((done (control-structure-find fs :do 1)))
    (add-to-definition fs
      `(flush-optimizer-stack)
      `(stack-pop loop-stack)
      `(stack-pop loop-stack))
    (execute-branch fs done)))

(define-word literal (:word "LITERAL" :immediate? t :compile-only? t)
  "( x -- )"
  "Compile X into the current definition. When executed, push X onto the data stack"
  (let ((value (stack-pop data-stack)))
    (add-to-definition fs
      `(stack-push data-stack ,value))))

(define-word loop (:word "LOOP" :immediate? t :compile-only? t)
  "Increment the index value by one and compare it with the limit value. If the index value is equal to the limit value,"
  "the loop is terminated, the parameters are discarded, and execution resumes with the next word. Otherwise, control"
  "returns to the word that follows the DO or ?DO that opened the loop"
  (verify-control-structure fs :do 2)
  (let ((again (stack-cell control-flow-stack 0))
        (done (stack-cell control-flow-stack 1)))
    (add-to-definition fs
      `(flush-optimizer-stack)
      `(incf (stack-cell loop-stack 0)))
    (execute-branch-when fs again
      (< (stack-cell loop-stack 0) (stack-cell loop-stack 1)))
    (add-to-definition fs
      `(stack-pop loop-stack)
      `(stack-pop loop-stack))
    (stack-pop control-flow-stack)
    (stack-pop control-flow-stack)
    (resolve-branch fs done)))

(define-word ash-left (:word "LSHIFT")
  "( x1 u -- n2 )"
  (let ((u (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (when (minusp u)
      (forth-exception :invalid-numeric-argument "Shift count can't be negative"))
    ;; The Forth standard defines LSHIFT as a logical left shift.
    ;; By treating the number as unsigned, we'll get the proper result.
    (stack-push data-stack (cell-signed (ash (cell-unsigned x1) u)))))

(define-word multiply-double (:word "M*")
  "( n1 n2 -- d )"
  "Multiply the signed integers N1 and N2 and push the resulting double precision integer D onto the data stack"
  (stack-push-double data-stack (* (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack)))))

(define-word max (:word "MAX")
  "( n1 n2 -- n3)"
  "Push the larger of N1 and N2 onto the data stack"
  (stack-push data-stack (max (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack)))))

(define-word min (:word "MIN")
  "( n1 n2 -- n3)"
  "Push the smaller of N1 and N2 onto the data stack"
  (stack-push data-stack (min (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack)))))

(define-word mod (:word "MOD")
  "( n1 n2 -- n3 )"
  (let ((n2 (cell-signed (stack-pop data-stack)))
        (n1 (cell-signed (stack-pop data-stack))))
    (if (zerop n2)
        (forth-exception :divide-by-zero)
        ;; Lisp MOD uses FLOOR to produce its result but, as we're using symmetric division througout,
        ;; we have to use REM which uses TRUNCATE here to preserve Forth semantics.
        (stack-push data-stack (cell-signed (rem n1 n2))))))

(define-word move-memory (:word "MOVE")
  "( addr1 addr2 u -- )"
  "Copy U bytes starting from a source starting at the address ADDR1 to the destination starting at address ADDR2"
  (let ((count (cell-signed (stack-pop data-stack)))
        (destination (stack-pop data-stack))
        (source (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to MOVE can't be negative"))
    (memory-copy memory source destination count)))

(define-word negate (:word "NEGATE")
  "( n1 -- n2 )"
  "Change the sign of the top of the data stack"
  (stack-push data-stack (- (cell-signed (stack-pop data-stack)))))

(define-word or (:word "OR")
  "( x1 x2 -- x3 )"
  "Return the bitwise logical inclusive or of X1 with X2"
  (stack-push data-stack (cell-unsigned (logior (stack-pop data-stack) (stack-pop data-stack)))))

(define-word stack-over (:word "OVER")
  "( x1 x2 -- x1 x2 x1 )"
  "Place a copy of X1 onto the top of the data stack"
  (stack-over data-stack))

(define-word postpone (:word "POSTPONE" :immediate? t :compile-only? t)
  "IMMEDIATE <name>"
  "At compile time, add the compilation behavior of NAME, rather than its execution behavior, to the current definition"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      (postpone fs word))))

(define-word reset-interpreter (:word "QUIT")
  "(S: i*x -- ) (R: j*x -- )"
  "Empty the data and return stacks, reset the input source to the terminal, restart the interpreter loop."
  "Do not display a message."
  (forth-exception :quit))

(define-word move-from-return-stack (:word "R>")
  "(S: -- x ) (R: x -- )"
  "Pop the top item off the return stack and push it onto the data stack"
  (stack-push data-stack (stack-pop return-stack)))

(define-word copy-from-return-stack (:word "R@")
  "(S: -- x ) (R: x -- x )"
  "Place a copy of the top item on the return stack onto the data stack"
  (stack-underflow-check return-stack)
  (stack-push data-stack (stack-cell return-stack 0)))

(define-word recurse (:word "RECURSE" :immediate? t :compile-only? t)
  "Append the execution behavior of the current definition to the current definition, so that it calls itself recursively"
  (add-to-definition fs
    `(forth-call fs ,(definition-word definition) ,(next-psuedo-pc definition))))

(define-word repeat (:word "REPEAT" :immediate? t :compile-only? t)
  "In a BEGIN ... WHILE ... REPEAT structure, unconditionally branch back to the location following the nearest previous BEGIN"
  (verify-control-structure fs :begin 2)
  (let ((again (stack-pop control-flow-stack))
        (done (stack-pop control-flow-stack)))
    (add-to-definition fs
      `(flush-optimizer-stack))
    (execute-branch fs again)
    (resolve-branch fs done)))

(define-word stack-rot (:word "ROT")
  "( x1 x2 x3 -- x2 x3 x1 )"
  "Rotate the top three items on the stack"
  (stack-rot data-stack))

(define-word ash-right (:word "RSHIFT")
  "( x1 u -- n2 )"
  (let ((u (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (when (minusp u)
      (forth-exception :invalid-numeric-argument "Shift count can't be negative"))
    ;; The Forth standard defines RSHIFT as a logical right shift.
    ;; By treating the number as unsigned, we'll get the proper result.
    (stack-push data-stack (cell-signed (ash (cell-unsigned x1) (- u))))))

;;; S" extended by the File-Access word set

(define-word single-to-double (:word "S>D")
  "( n -- d )"
  "Convert the signed integer N to a signed double precision integer D"
  (stack-push-double data-stack (cell-signed (stack-pop data-stack))))

(define-word add-sign-to-picture-buffer (:word "SIGN")
  "( n -- )"
  "If N is negative, add a minus sign to the beginning of the pictured numeric output string"
  (unless (pictured-buffer-active? (memory-pictured-buffer memory))
    (forth-exception :no-pictured-output "Can't use SIGN outside <# ... #>"))
  (when (minusp (cell-signed (stack-pop data-stack)))
    (add-to-pictured-buffer (memory-pictured-buffer memory) (forth-char #\-))))

(define-word truncate-mod (:word "SM/REM")
  "( d n1 -- n2 n3 )"
  "Divide the double precision integer D by the integer N1 using symmetric division"
  "Push the remainder N2 and quotient N3 onto the data stack"
  (let ((n1 (cell-signed (stack-pop data-stack)))
        (d (stack-pop-double data-stack)))
    (if (zerop n1)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate d n1)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word source (:word "SOURCE")
  "( -- c-addr u )"
  "Return the address C-ADDR and size U of the input buffer"
  (multiple-value-bind (address count)
      (access-source-buffer files)
    (stack-push data-stack address)
    (stack-push data-stack count)))

(define-word space (:word "SPACE")
  "Write a space to the terminal"
  (write-char #\Space))

(defparameter +spaces+ "                                ")

(define-word spaces (:word "SPACES")
  "( u -- )"
  "Write U spaces to the terminal"
  (let ((count (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to SPACES can't be negative"))
    (loop with n-spaces = (length +spaces+)
          for n = count then (- n n-spaces)
          while (plusp n)
          do (write-string (subseq +spaces+ 0 (min n n-spaces))))))

;;; STATE extended by the Programming-Tools word set

(define-word stack-swap (:word "SWAP")
  "( x1 x2 -- x2 x1 )"
  "Exchange the top two items on the data stack"
  (stack-swap data-stack))

(define-word then (:word "THEN" :immediate? t :compile-only? t)
  "Mark the point at which the true and false portions of an IF structure merge"
  (verify-control-structure fs :if)
  (let ((branch (stack-pop control-flow-stack)))
    (resolve-branch fs branch)))

(define-word write-string (:word "TYPE")
  " ( a-addr u -- )"
  "Type the string at A-ADDR of length U on the terminal"
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to TYPE can't be negative"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address count)
      (write-string (forth-string-to-native forth-memory offset count)))))

(define-word print-tos-unsigned (:word "U.")
  "( u -- )"
  "Display the top cell of the data stack as an unsigned integer in the current base"
  (let ((value (cell-unsigned (stack-pop data-stack))))
    (format t "~VR " base value)))

(define-word less-than-unsigned (:word "U<")
  " ( u1 u2 -- flag )"
  "Return true if U1 is less than U2"
  ;; As the first value popped off the stack is U2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (> (cell-unsigned (stack-pop data-stack)) (cell-unsigned (stack-pop data-stack)))
                             +true+ +false+)))

(define-word unsigned-multiply-double (:word "UM*")
  "( u1 u2 -- ud )"
  "Multiply the unsigned integers U1 and U2 and push the resulting unsigned double precision integer UD onto the data stack"
  (let ((u2 (cell-unsigned (stack-pop data-stack)))
        (u1 (cell-unsigned (stack-pop data-stack))))
    (stack-push-double data-stack (* u1 u2))))

(define-word unsigned-floor-mod (:word "UM/MOD")
  "( ud u1 -- u2 u3 )"
  "Divide the unsigned double precision integer UD by the unsigned integer U1"
  "Push the unsigned remainder U2 and unsigned quotient U3 onto the data stack"
  (let ((u1 (cell-unsigned (stack-pop data-stack)))
        (ud (stack-pop-double-unsigned data-stack)))
    (if (zerop u1)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate ud u1)
          (stack-push data-stack (cell-unsigned remainder))
          (stack-push data-stack (cell-unsigned quotient))))))

(define-word unloop (:word "UNLOOP" :immediate? t :compile-only? t)
  "Discard the loop parameters for the current nesting level. This word is not needed when a DO ... LOOP completes normally,"
  "but it is required before leaving a definition by calling EXIT. One UNLOOP call for each level of loop nesting is required"
  "before leaving a definition."
  (add-to-definition fs
    `(flush-optimizer-stack)
    `(stack-pop loop-stack)
    `(stack-pop loop-stack)))

(define-word until (:word "UNTIL" :immediate? t :compile-only? t)
  "If X is zero, branch back to the location immediately following the nearest previous BEGIN; otherwise, continue"
  "execution beyond the UNTIL"
  (verify-control-structure fs :begin)
  (add-to-definition fs
    `(flush-optimizer-stack))
  (execute-branch-when fs (stack-pop control-flow-stack)
    (falsep (cell-unsigned (stack-pop data-stack)))))

(define-word variable (:word "VARIABLE")
  "VARIABLE <name>"
  "Allocate a cell in data space and create a dictionary entry for <name> which returns the address of that cell"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (allocate-memory memory +cell-size+))
           (word (make-word name #'push-parameter-as-cell :parameters (make-parameters address) :created-word? t)))
      (setf (word-inline-forms word) `((stack-push data-stack ,address))
            (word-inlineable? word) t)
      (add-and-register-word fs word address))))

(define-word while (:word "WHILE" :immediate? t :compile-only? t)
  "( x -- )"
  "If X is zero, branch to the location immediately following the nearest REPEAT; otherwise, continue"
  "execution beyond the WHILE"
  (verify-control-structure fs :begin)
  (let ((branch (make-branch-reference :begin)))
    (stack-push control-flow-stack branch)
    (stack-roll control-flow-stack 1)
    (add-to-definition fs
      '(flush-optimizer-stack))
    (execute-branch-when fs branch
      (falsep (cell-unsigned (stack-pop data-stack))))))

(define-word word (:word "WORD")
  "WORD <text>" "( char -- c-addr )"
  "Skip any leading occurences of the delimiter character CHAR. Parse TEXT delimited by CHAR."
  "Return C-ADDR, the address of a temporary location containing the parsed text as a counted string"
  (let* ((char (stack-pop data-stack))
         (text (or (word files (native-char char)) ""))
         (length (length text))
         (word-space (memory-word-space memory))
         (address (transient-space-base-address memory word-space)))
    ;; Length of a counted string is always a single byte regardless of character size
    (ensure-transient-space-holds memory word-space (1+ (* length +char-size+)))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address (1+ (* length +char-size+)))
      (native-into-forth-counted-string text forth-memory offset))
    (stack-push data-stack address)
    (seal-transient-space memory word-space)))

(define-word xor (:word "XOR")
  "( x1 x2 -- x3 )"
  "Return the bitwise logical exclusive or of X1 with X2"
  (stack-push data-stack (cell-unsigned (logxor (stack-pop data-stack) (stack-pop data-stack)))))

(define-word interpret (:word "[" :immediate? t :compile-only? t)
  "Temporarily switch from compiling a definition to interpreting words"
  (setf compiling-paused? t)
  (setf (state fs) :interpreting))

(define-word lookup-xt-compiled (:word "[']" :immediate? t :compile-only? t)
  "' <name>" "( -- xt )"
  "Lookup NAME and fetch its execution token. Append code to the current definition to push the execution token onto the stack"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      (add-to-definition fs
        `(stack-push data-stack ,(xt-token (word-execution-token word)))))))

(define-word compile-char (:word "[CHAR]" :immediate? t :compile-only? t)
  "[CHAR] <c>" "( -- char )"
  "When compiling a definition, parse the next word, usually a single character, and compile the ASCII value"
  "of its first character as a literal which will be pushed onto the data stack when the definition is executed"
  (let ((char (word files #\Space)))
    (when (null char)
      (forth-exception :zero-length-name))
    (add-to-definition fs
      `(stack-push data-stack ,(forth-char (aref char 0))))))

(define-word compile (:word "]")
  "Switch back to compiling a definition after using ']'"
  (unless (shiftf compiling-paused? nil)
    (forth-exception :not-compiling "Can't resume compiling when nothing's being compiled"))
  (setf (state fs) :compiling))


;;; Core extension words as defined in Section 6 of the Forth 2012 specification

(define-word displayed-comment (:word ".(" :immediate? t)
  "Display without interpretation all text up to the next close parenthesis on the console"
  (let ((comment (parse files #\))))
    (write-string comment)))

(define-word print-tos-in-field (:word ".R")
  "( n1 +n2 -- )"
  "Display N1 right aligned in a field N2 characters wide. If the number of characters required to display N1"
  "is greater than N2, all digits are displayed in a field as wide as necessary with no leading spaces"
  (let ((width (stack-pop data-stack))
        (value (cell-signed (stack-pop data-stack))))
    (when (minusp width)
      (forth-exception :invalid-numeric-argument "Field width to .R can't be negative"))
    (format t "~V,VR" base width value)))

(define-word not-zerop (:word "0<>")
  " ( n -- flag )"
  "Return true if N is not equal to zero"
  (stack-push data-stack (if (zerop (cell-signed (stack-pop data-stack))) +false+ +true+)))

(define-word plusp (:word "0>")
  " ( n -- flag )"
  "Return true if N is greater than zero"
  (stack-push data-stack (if (plusp (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word move-two-to-return-stack (:word "2>R")
  "(S: x1 x2 -- ) (R: -- x1 x2 )"
  "Pop the top two items off the data stack and push them onto the return stack"
  (let ((x2 (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (stack-push return-stack x1)
    (stack-push return-stack x2)))

(define-word move-two-from-return-stack (:word "2R>")
  "(S: -- x1 x2 ) (R: x1 x2 -- )"
  "Pop the top two items off the return stack and push them onto the data stack"
  (let ((x2 (stack-pop return-stack))
        (x1 (stack-pop return-stack)))
    (stack-push data-stack x1)
    (stack-push data-stack x2)))

(define-word copy-two-from-return-stack (:word "2R@")
  "(S: -- x1 x2 ) (R: x1 x2 -- x1 x2 )"
  "Place a copy of the top two items on the return stack onto the data stack"
  (stack-underflow-check return-stack 2)
  (stack-push data-stack (stack-cell return-stack 1))
  (stack-push data-stack (stack-cell return-stack 0)))

(define-word start-anonymous-definition (:word ":NONAME")
  "( -- xt )"
  "Create an execution token XT, enter compilation state and start the current definition"
  "This definition can be executed later by using XT EXECUTE"
  (stack-push data-stack (begin-compilation fs)))

(define-word not-equal (:word "<>")
  " ( n1 n2 -- flag )"
  "Return true if N1 is not equal to N2"
  (stack-push data-stack (if (/= (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word maybe-do (:word "?DO" :immediate? t :compile-only? t)
  "( n1 n2 -- )"
  "Like DO, but check whether the limit value and initial loop index are equal. If they are, continue execution immediately"
  "following the next LOOP or +LOOP; otherwise, set up the loop values and continue execution immediately following ?DO"
  (let ((again (make-branch-reference :do))
        (done (make-branch-reference :do)))
    (stack-push control-flow-stack done)
    (stack-push control-flow-stack again)
    (add-to-definition fs
      `(flush-optimizer-stack)
      `(stack-underflow-check data-stack 2))
    (execute-branch-when fs done
      (and (= (stack-cell data-stack 0) (stack-cell data-stack 1))
           (prog1
               t
             ;; Be sure to pop the limit and initial index when they're equal
             (stack-2drop data-stack))))
    (add-to-definition fs
      `(let ((n2 (stack-pop data-stack))
             (n1 (stack-pop data-stack)))
         (stack-push loop-stack n1)
         (stack-push loop-stack n2)))
    (resolve-branch fs again)))

(define-word action-of (:word "ACTION-OF" :immediate? t)
  "ACTION-OF <name>" "( -- xt )"
  "If interpreted, place the execution token (xt) that NAME is set to execute onto the stack"
  "If compiled, add code to the current definition to place the execution token that NAME is set to execute"
  "when this definition is executed onto the stack"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      (unless (word-deferring-word? word)
        (forth-exception :not-defer "~A was not created by DEFER" name))
      (case (state fs)
        (:interpreting
         (let ((xt (parameters-p1 (word-parameters word))))
           (when (null xt)
             (forth-exception :defer-not-set))
           (stack-push data-stack xt)))
        (:compiling
         (add-to-definition fs
           `(let ((xt (parameters-p1 (word-parameters ,word))))
              (when (null xt)
                (forth-exception :defer-not-set))
              (stack-push data-stack xt))))))))

(define-word again (:word "AGAIN" :immediate? t :compile-only? t)
  "Unconditionally branch back to the point immediately following the nearest previous BEGIN"
  (verify-control-structure fs :begin)
  (execute-branch fs (stack-pop control-flow-stack)))

(define-word create-buffer (:word "BUFFER:")
  "BUFFER: <name>" "( u -- )"
  "Reserve U bytes of memory at an aligned address and create a dictionary entry for <NAME>"
  "that returns the address of the first byte"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((count (stack-pop data-stack))
           (address (allocate-memory memory count))
           (word (make-word name #'push-parameter-as-cell :parameters (make-parameters address) :created-word? t)))
      (setf (word-inline-forms word) `((stack-push data-stack ,address))
            (word-inlineable? word) t)
      (add-and-register-word fs word address))))

(define-word counted-string (:word "C\"" :immediate? t :compile-only? t)
  "C\" <text>\"" "( -- a-addr )"
  "Compile TEXT as a counted string into the current definition. When executed, place its address on the data stack"
  (let* ((text (parse files #\"))
         (text-size (* (length text) +char-size+))
         ;; Length of a counted string is always a single byte regardless of character size
         (address (allocate-memory memory (1+ text-size))))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address (1+ text-size))
      (native-into-forth-counted-string text forth-memory offset)
      (add-to-definition fs
        `(stack-push data-stack ,address)))))

(define-word case (:word "CASE" :immediate? t :compile-only? t)
  "Mark the start of a CASE ... OF ... ENDOF ... CASE structure"
  (let ((branch (make-branch-reference :case)))
    ;; This will be used to branch past the ENDCASE
    (stack-push control-flow-stack branch)))

(define-word compile-comma (:word "COMPILE,")
  "( xt -- )"
  "Append the execution semantics of the definition represented by XT to the execution semantics of the current definition"
  (let ((xt (stack-pop data-stack)))
    (compile-comma fs xt)))

(define-word defer (:word "DEFER")
  "DEFER <name>"
  "Create a definition for NAME that will execute the execution token stored in NAME via IS or DEFER!"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'execute-parameter :deferring-word? t :parameters (make-parameters nil))))
      (add-and-register-word fs word))))

(define-word defer! (:word "DEFER!")
  "(xt2 xt1 -- )"
  "Set the word XT1 to execute XT2. XT1 must be the execution token of a word created by DEFER"
  (let* ((xt1 (stack-pop data-stack))
         (word (find-word execution-tokens xt1))
         (xt2 (stack-pop data-stack)))
    (unless (word-deferring-word? word)
      (forth-exception :not-defer "~A was not created by DEFER" (word-name word)))
    (verify-execution-token execution-tokens xt2)
    (setf (parameters-p1 (word-parameters word)) xt2)))

(define-word defer@ (:word "DEFER@")
  "( xt1 -- xt2 )"
  "XT2 is the execution token XT1 is set to execute. XT1 must be the execution token of a word created by DEFER"
  (let* ((xt1 (stack-pop data-stack))
         (word (find-word execution-tokens xt1)))
    (unless (word-deferring-word? word)
      (forth-exception :not-defer "~A was not created by DEFER" (word-name word)))
    (let ((xt (parameters-p1 (word-parameters word))))
      (when (null xt)
        (forth-exception :defer-not-set))
      (stack-push data-stack xt))))

(define-word endcase (:word "ENDCASE" :immediate? t :compile-only? t)
  "( x -- )"
  "Discard the top stack value X (presumably the case selector) and continue execution"
  (verify-control-structure fs :case)
  (let ((branch (stack-pop control-flow-stack)))
    (add-to-definition fs
      `(flush-optimizer-stack)
      `(stack-drop data-stack))
    (resolve-branch fs branch)))

(define-word endof (:word "ENDOF" :immediate? t :compile-only? t)
  "Unconditionally branch to immediately beyond the next ENDCASE"
  (verify-control-structure fs :case 2)
  (let ((branch (stack-pop control-flow-stack)))
    ;; Branch past the ENDCASE
    (execute-branch fs (stack-cell control-flow-stack 0))
    (resolve-branch fs branch)))

(define-word erase-memory (:word "ERASE")
  "( c-addr u -- )"
  "Set a region of memory, at address C-ADDR and of length U, to zero"
  (let ((count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to ERASE can't be negative"))
    (memory-fill memory address count 0)))

(define-word false (:word "FALSE")
  "( -- flag )"
  "Return a FLAG that is false"
  (stack-push data-stack +false+))

(define-word hex (:word "HEX")
  "Change the system base to hexadecimal"
  (flush-optimizer-stack :contains (state-slot-address memory 'base))
  (setf base 16.))

(define-word add-string-to-picture-buffer (:word "HOLDS")
  "( c-addr u -- )"
  "Adds the string represented by C-ADDR U to the pictured numeric output string"
  (unless (pictured-buffer-active? (memory-pictured-buffer memory))
    (forth-exception :no-pictured-output "Can't use HOLDS outside <# ... #>"))
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to HOLDS can't be negative"))
    (add-string-to-pictured-buffer (memory-pictured-buffer memory) memory address count)))

(define-word is (:word "IS" :immediate? t)
  "IS <name>" "( xt -- )"
  "If interpreting, set NAME to execute XT"
  "If compiling, add code to the current definition to set NAME to execute XT"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      (unless (word-deferring-word? word)
        (forth-exception :not-defer "~A was not created by DEFER" name))
      (case (state fs)
        (:interpreting
         (let ((xt (stack-pop data-stack)))
           (verify-execution-token execution-tokens xt)
           (setf (parameters-p1 (word-parameters word)) xt)))
        (:compiling
         (add-to-definition fs
           `(let ((xt (stack-pop data-stack)))
              (verify-execution-token execution-tokens xt)
              (setf (parameters-p1 (word-parameters ,word)) xt))))))))

(define-word marker (:word "MARKER")
  "MARKER <name>"
  "Define NAME which restores all dictionary allocation and search order pointers to the state they had just prior"
  "to the definition of NAME. Remove the definition of NAME and all subsequent definitions. Restoration of any structures"
  "still existing that could refer to deleted definitions or deallocated data space is not necessarily provided."
  "No other contextual information such as numeric base is affected"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'do-marker))
          (marker (register-marker word-lists)))
      (add-and-register-word fs word)
      (setf (parameters-p1 (word-parameters word)) marker))))

(define-word stack-nip (:word "NIP")
  "( x1 x2 -- x2 )"
  "Drop the second item on the data stack, leaving the top unchanged"
  (stack-nip data-stack))

(define-word of (:word "OF" :immediate? t :compile-only? t)
  "( x1 x2 -- | x1 )"
  "If the test value X2 is not equal to the case selector X1, discard X2 and branch forward to the code"
  "immediately following the next ENDOF; otherwise, discard both values and continue execution beyond the OF "
  (verify-control-structure fs :case)
  (let ((branch (make-branch-reference :case)))
    ;; This will be used to branch past the matching ENDOF
    (stack-push control-flow-stack branch)
    ;;#+TODO
    (add-to-definition fs
      '(flush-optimizer-stack))
    (execute-branch-when fs branch
      (not (= (stack-pop data-stack) (stack-cell data-stack 0))))
    (add-to-definition fs
      `(stack-drop data-stack))))

(define-word pad (:word "PAD")
  "( -- a-addr )"
  "Return the address of a temporary storage area usually used for processing strings"
  (stack-push data-stack (pad-base-address memory)))

(define-word parse (:word "PARSE")
  "PARSE <text>" "( char -- c-addr u )"
  "Parse TEXT to the first instance of CHAR."
  "C-ADDR is the address (within the input buffer) and U is the length of the parsed string"
  (multiple-value-bind (address length)
      (parse files (native-char (stack-pop data-stack)) :forth-values? t)
    (stack-push data-stack address)
    (stack-push data-stack length)))

(define-word parse-name (:word "PARSE-NAME")
  "PARSE-NAME <name>" "( -- c-addr u )"
  "Skip leading space delimiters. Parse NAME delimited by a space"
  "C-ADDR is the address of the selected string within the input buffer and U is its length in characters"
  (multiple-value-bind (address length)
      (word files #\Space :forth-values? t)
    (stack-push data-stack address)
    (stack-push data-stack length)))

(define-word stack-pick (:word "PICK")
  "( +n -- x )"
  "Place a copy of the Nth stack item onto the top of the data stack"
  (let ((n (stack-pop data-stack)))
    (when (minusp n)
      (forth-exception :invalid-numeric-argument "Pick count can't be negative"))
    (stack-pick data-stack n)))

;;; REFILL extended by File-Access word set

(define-word restore-input (:word "RESTORE-INPUT")
  "( x1 ... xn n -- flag )"
  "Attempt to restore the input source specification to the state described by X1 through XN."
  "FLAG is true if the input source specification cannot be so restored"
  (let* ((n (stack-pop data-stack))
         (state-vector (make-array n :initial-element 0)))
    (dotimes (i n)
      (setf (aref state-vector (- n i 1)) (stack-pop data-stack)))
    ;; Unlike the word, the RESTORE-INPUT method returns T for success
    (if (restore-input files state-vector)
        (stack-push data-stack +false+)
        (stack-push data-stack +true+))))

(define-word stack-roll (:word "ROLL")
  "( x(u) x(u-1) . . . x(0) u -- x(u-1) . . . x(0) x(u) )"
  "Remove U. Rotate U+1 items on the top of the stack"
  (let ((n (stack-pop data-stack)))
    (when (minusp n)
      (forth-exception :invalid-numeric-argument "Roll count can't be negative"))
    (stack-roll data-stack n)))

;;; S\" extended by File-Access word set

(define-word save-input (:word "SAVE-INPUT")
  "( -- x1 ... xn n )"
  "X1 through XN describe the current state of the input source specification for later use by RESTORE-INPUT"
  (let ((state-vector (save-input files)))
    (let ((n (length state-vector)))
      (dotimes (i n)
        (stack-push data-stack (aref state-vector i)))
      (stack-push data-stack n))))

;;; SOURCE-ID extended by the File-Access word set

(define-word to (:word "TO" :immediate? t :inlineable? nil)
  "TO <name>" "( x | x1 x2 -- ) or (F: r -- )"
  "Store X in the data space associated with <name> which must have been created with VALUE or"
  "store X1 X2 in the data space associated with <name> which must have been created with 2VALUE or"
  "store R in the data space associated with <name> which must have been created with FVALUE"
  (case (state fs)
    (:interpreting
     (let* ((name (word files #\Space))
            (word (lookup word-lists name)))
       (when (null name)
         (forth-exception :zero-length-name))
       (when (null word)
         (forth-exception :undefined-word "~A is not defined" name))
       (let ((address (parameters-p1 (word-parameters word)))
             (type (parameters-p2 (word-parameters word))))
         (unless (member type '(:value :2value :fvalue))
           (forth-exception :invalid-name-argument "~A was not created by VALUE, 2VALUE, or FVALUE" name))
         (case type
           (:value
            (setf (memory-cell memory address) (stack-pop data-stack)))
           (:2value
            (setf (memory-double-cell memory address) (stack-pop-double data-stack)))
           (:fvalue
            (setf (memory-native-float memory address) (stack-pop float-stack)))))))
    (:compiling
     (let* ((name (word files #\Space))
            (local (find name (locals-locals (definition-locals definition)) :test #'string-equal :key #'local-name))
            (word (lookup word-lists name)))
       (when (null name)
         (forth-exception :zero-length-name))
       (when (and (null local) (null word))
         (forth-exception :undefined-word "~A is not defined" name))
       (if local
           (add-to-definition fs
             ;;#+TODO
             `(flush-optimizer-stack)
             `(setf ,(local-symbol local) (stack-pop data-stack)))
           (let ((address (parameters-p1 (word-parameters word)))
                 (type (parameters-p2 (word-parameters word))))
             (unless (member type '(:value :2value :fvalue))
               (forth-exception :invalid-name-argument "~A was not created by VALUE, 2VALUE, or FVALUE" name))
             (case type
               (:value
                (add-to-definition fs
                  `(setf (memory-cell memory ,address) (stack-pop data-stack))))
               (:2value
                (add-to-definition fs
                  `(setf (memory-double-cell memory ,address) (stack-pop-double data-stack))))
               (:fvalue
                (add-to-definition fs
                  `(setf (memory-native-float memory ,address) (stack-pop float-stack)))))))))))

(define-word true (:word "TRUE")
  "( -- flag )"
  "Return a FLAG that is true"
  (stack-push data-stack +true+))

(define-word stack-tuck (:word "TUCK")
  "( x1 x2 -- x2 x1 x2 )"
  "Place a copy of the top item on the data stack below the second item on the stack"
  (stack-tuck data-stack))

(define-word print-tos-unsigned-in-field (:word "U.R")
  "( u +n -- )"
  "Display U right aligned in a field N characters wide. If the number of characters required to display U"
  "is greater than N, all digits are displayed in a field as wide as necessary with no leading spaces"
  (let ((width (stack-pop data-stack))
        (value (cell-unsigned (stack-pop data-stack))))
    (when (minusp width)
      (forth-exception :invalid-numeric-argument "Field width to U.R can't be negative"))
    (format t "~V,VR" base width value)))

(define-word greater-than-unsigned (:word "U>")
  " ( u1 u2 -- flag )"
  "Return true if U1 is greater than U2"
  ;; As the first value popped off the stack is U2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (< (cell-unsigned (stack-pop data-stack)) (cell-unsigned (stack-pop data-stack))) +true+ +false+)))

(define-word unused (:word "UNUSED")
  "( -- u )"
  "U is the amount of space remaining in the region addressed by HERE, in address units"
  (stack-push data-stack (data-space-unused memory)))

(define-word value (:word "VALUE")
  "VALUE <name>" "( x -- )"
  "Allocate a cell in data space, initialize it to X, and create a dictionary entry for <name> which returns"
  "the contents of that cell in data space. To change the value, use TO"
  (let ((name (word files #\Space))
        (value (stack-pop data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (allocate-memory memory +cell-size+))
           (word (make-word name #'push-value :parameters (make-parameters address :value) :created-word? t)))
      (setf (memory-cell memory address) value)
      (add-and-register-word fs word address))))

(define-word within (:word "WITHIN")
  "( x1 x2 x3 -- flag )"
  "Returns true if X2 <= X1 < X3. X1, X2, and X3 should be either all signed or all unsigned"
  "Our implementation simulates a machine that ignores arithmeitc overflow. See the Forth 2012"
  "Standard, Section A.6.2.2440, for an explanation of how to handle this situation"
  (stack-over data-stack)
  (let* ((x2 (stack-pop data-stack))
         (x3 (stack-pop data-stack))
         (x3-x2 (cell-unsigned (- x3 x2)))
         (x2 (stack-pop data-stack))
         (x1 (stack-pop data-stack))
         (x1-x2 (cell-unsigned (- x1 x2))))
    (if (< x1-x2 x3-x2)
        (stack-push data-stack +true+)
        (stack-push data-stack +false+))))

(define-word [compile] (:word "[COMPILE]" :immediate? t :compile-only? t)
  "[COMPILE] <name>"
  "Skip leading space delimiters. Parse NAME delimited by a space. Find NAME. If NAME has other than default compilation"
  "semantics, append them to the current definition; otherwise append the execution semantics of NAME"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (lookup word-lists name)))
      (when (null word)
        (forth-exception :undefined-word "~A is not defined" name))
      ;; According to Section A.6.1.2033 of the Forth 2012 specification, [COMPILE] has the same semantics as POSTPONE
      ;; but was only intended to be applied to immediate words. So, we'll just treat it as identical to POSTPONE.
      (postpone fs word))))

(define-word rest-of-line-comment (:word "\\" :immediate? t)
  "Ignore all text on the rest of the line"
  (flush-input-line files))
