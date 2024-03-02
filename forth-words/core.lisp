(in-package #:forth)

;;; Paragraph numbers refer to the Forth Programmer's Handbook, 3rd Edition

;;; 1.1.6 Numeric input

(define-state-word base)

(define-word decimal ()
  "Change the system base to decimal"
  (setf base 10.))

(define-word hex ()
  "Change the system base to hexadecimal"
  (setf base 16.))


;;; 1.4.1 Comments

(define-word comment (:word "(")
  "Ignore all text up to and including the next close parenthesis"
  (word files #\)))

(define-word displayed-comment (:word ".(")
  "Display without interpretation all text up to the next close parenthesis on the console"
  (let ((comment (word files #\))))
    (write-line comment)))

(define-word rest-of-line-comment (:word "\\")
  "Ignore all text on the rest of the line"
  (flush-input-line files))


;;; 2.1.2 Data Stack Manipulation

;;; 2.1.2.1 Single-Item Operators

(define-word stack-?dup (:word "?DUP")
  "( x - 0 | x x )"
  "Conditionally duplicate the top of the data stack if it is non-zero"
  (stack-?dup data-stack))

(define-word stack-depth (:word "DEPTH")
  "( - +n )"
  "Pushes the current depth of the data stack onto the data stack"
  (stack-push data-stack (stack-depth data-stack)))

(define-word stack-drop (:word "DROP")
  "( x - )"
  "Remove the top entry from the data stack"
  (stack-drop data-stack))

(define-word stack-dup (:word "DUP")
  "( x - x x )"
  "Duplicate the top of the data stack"
  (stack-dup data-stack))

(define-word stack-nip (:word "NIP")
  "( x1 x2 - x2 )"
  "Drop the second item on the data stack, leaving the top unchanged"
  (stack-nip data-stack))

(define-word stack-over (:word "OVER")
  "( x1 x2 - x1 x2 x1 )"
  "Place a copy of X1 onto the top of the data stack"
  (stack-over data-stack))

(define-word stack-pick (:word "PICK")
  "( +n - x )"
  "Place a copy of the Nth stack item onto the top of the data stack"
  (let ((n (stack-pop data-stack)))
    (when (minusp n)
      (forth-exception :invalid-numeric-argument "Pick count can't be negative"))
    (stack-pick data-stack n)))

(define-word stack-rot (:word "ROT")
  "( x1 x2 x3 - x2 x3 x1 )"
  "Rotate the top three items on the stack"
  (stack-rot data-stack))

(define-word stack-swap (:word "SWAP")
  "( x1 x2 - x2 x1 )"
  "Exchange the top two items on the data stack"
  (stack-swap data-stack))

(define-word stack-tuck (:word "TUCK")
  "( x1 x2 - x2 x1 x2 )"
  "Place a copy of the top item on the data stack below the second item on the stack"
  (stack-tuck data-stack))

;;; 2.1.2.2 Two-Item Operators

(define-word stack-2drop (:word "2DROP")
  "( x1 x2 - )"
  "Remove the top pair of cells from the data stack"
  (stack-2drop data-stack))

(define-word stack-2dup (:word "2DUP")
  "( x1 x2 - x1 x2 x1 x2 )"
  "Duplicate the top cell pair on the data stack"
  (stack-2dup data-stack))

(define-word stack-2over (:word "2OVER")
  "( x1 x2 x3 x4 - x1 x2 x3 x4 x1 x2 )"
  "Copy cell pair X1 X2 to the top of the data stack"
  (stack-2over data-stack))

(define-word stack-2swap (:word "2SWAP")
  "( x1 x2 x3 x4 - x3 x4 x1 x2 )"
  "Exchange the top two cell pairs on the data stack"
  (stack-2swap data-stack))


;;; 2.1.3 Return Stack Manipulation

(define-word move-two-to-return-stack (:word "2>R")
  "(S: x1 x2 - ) (R: - x1 x2 )"
  "Pop the top two items off the data stack and push them onto the return stack"
  (let ((x2 (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (stack-push return-stack x1)
    (stack-push return-stack x2)))

(define-word move-two-from-return-stack (:word "2R>")
  "(S: - x1 x2 ) (R: x1 x2 - )"
  "Pop the top two items off the return stack and push them onto the data stack"
  (let ((x2 (stack-pop return-stack))
        (x1 (stack-pop return-stack)))
    (stack-push data-stack x1)
    (stack-push data-stack x2)))

(define-word copy-two-from-return-stack (:word "2R@")
  "(S: - x1 x2 ) (R: x1 x2 - x1 x2 )"
  "Place a copy of the top two items on the return stack onto the data stack"
  (stack-underflow-check return-stack 2)
  (stack-push data-stack (stack-cell return-stack 1))
  (stack-push data-stack (stack-cell return-stack 0)))

(define-word move-to-return-stack (:word ">R")
  "(S: x - ) (R: - x )"
  "Pop the top item off the data stack and push it onto the return stack"
  (stack-push return-stack (stack-pop data-stack)))

(define-word move-from-return-stack (:word "R>")
  "(S: - x ) (R: x - )"
  "Pop the top item off the return stack and push it onto the data stack"
  (stack-push data-stack (stack-pop return-stack)))

(define-word copy-from-return-stack (:word "R@")
  "(S: - x ) (R: x - x )"
  "Place a copy of the top item on the return stack onto the data stack"
  (stack-underflow-check return-stack)
  (stack-push data-stack (stack-cell return-stack 0)))


;;; 2.1.4 Programmer Conveniences

;;; ENVIRONMENT?


;;; 2.2.1 Arithmetic and Shift Operations

;;; Single-Precision Operations

(define-word add (:word "+")
  "( n1 n2 - n3 )"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (stack-push data-stack (cell-signed (+ n1 n2)))))

(define-word subtract (:word "-")
  "( n1 n2 - n3 )"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (stack-push data-stack (cell-signed (- n1 n2)))))

(define-word multiply (:word "*")
  "( n1 n2 - n3 )"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (stack-push data-stack (cell-signed (* n1 n2)))))

(define-word divide (:word "/")
  "( n1 n2 - n3 )"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (if (zerop n2)
        (forth-exception :divide-by-zero)
        (stack-push data-stack (cell-signed (truncate n1 n2))))))

(define-word multiply-divide (:word "*/")
  "( n1 n2 n3 - n4 )"
  (let ((n3 (stack-pop data-stack))
        (n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (stack-push data-stack (cell-signed (truncate (* n1 n2) n3)))))

(define-word multiply-divide-mod (:word "*/MOD")
  "( n1 n2 n3 - n4 n5 )"
  (let ((n3 (stack-pop data-stack))
        (n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (if (zerop n3)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate (* n1 n2) n3)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word divide-mod (:word "/MOD")
  "( n1 n2 - n3 n4 )"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (if (zerop n2)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate n1 n2)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word add-one (:word "1+")
  "( n1 - n2 )"
  (stack-push data-stack (cell-signed (1+ (stack-pop data-stack)))))

(define-word subtract-one (:word "1-")
  "( n1 - n2 )"
  (stack-push data-stack (cell-signed (1- (stack-pop data-stack)))))

(define-word add-two (:word "2+")
  "( n1 - n2 )"
  (stack-push data-stack (cell-signed (+ (stack-pop data-stack) 2))))

(define-word subtract-two (:word "2-")
  "( n1 - n2 )"
  (stack-push data-stack (cell-signed (- (stack-pop data-stack) 2))))

(define-word ash-left-1 (:word "2*")
  "( x1 - x2 )"
  (stack-push data-stack (cell-signed (ash (stack-pop data-stack) 1))))

(define-word ash-right-1 (:word "2/")
  "( x1 - x2 )"
  (stack-push data-stack (cell-signed (ash (stack-pop data-stack) -1))))

(define-word ash-left (:word "LSHIFT")
  "( x1 u - n2 )"
  (let ((u (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (when (minusp u)
      (forth-exception :invalid-numeric-argument "Shift count can't be negative"))
    ;; The Forth standard defines LSHIFT as a logical left shift.
    ;; By treating the number as unsigned, we'll get the proper result.
    (stack-push data-stack (cell-signed (ash (cell-unsigned x1) u)))))

(define-word mod (:word "MOD")
  "( n1 n2 - n3 )"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (if (zerop n2)
        (forth-exception :divide-by-zero)
        (stack-push data-stack (cell-signed (mod n1 n2))))))

(define-word ash-right (:word "RSHIFT")
  "( x1 u - n2 )"
  (let ((u (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (when (minusp u)
      (forth-exception :invalid-numeric-argument "Shift count can't be negative"))
    ;; The Forth standard defines RSHIFT as a logical right shift.
    ;; By treating the number as unsigned, we'll get the proper result.
    (stack-push data-stack (cell-signed (ash (cell-unsigned x1) (- u))))))

;;; Mixed-Precision Operations

(define-word floor-mod (:word "FM/MOD")
  "( d n1 - n2 n3 )"
  "Divide the double precision integer D by the integer N1 using floored division"
  "Push the remainder N2 and quotient N3 onto the data stack"
  (let ((n1 (stack-pop data-stack))
        (d (stack-pop-double data-stack)))
    (if (zerop n1)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (floor d n1)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word multiply-double (:word "M*")
  "( n1 n2 - d )"
  "Multiply the signed integers N1 and N2 and push the resulting double precision integer D onto the data stack"
  (let ((n2 (stack-pop data-stack))
        (n1 (stack-pop data-stack)))
    (stack-push-double data-stack (* n1 n2))))

(define-word single-to-double (:word "S>D")
  "( n - d )"
  "Convert the signed integer N to a signed double precision integer D"
  (stack-push-double data-stack (cell-signed (stack-pop data-stack))))

(define-word truncate-mod (:word "SM/MOD")
  "( d n1 - n2 n3 )"
  "Divide the double precision integer D by the integer N1 using symmetric division"
  "Push the remainder N2 and quotient N3 onto the data stack"
  (let ((n1 (stack-pop data-stack))
        (d (stack-pop-double data-stack)))
    (if (zerop n1)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate d n1)
          (stack-push data-stack (cell-signed remainder))
          (stack-push data-stack (cell-signed quotient))))))

(define-word floor-mod (:word "UM/MOD")
  "( ud u1 - u2 u3 )"
  "Divide the unsigned double precision integer UD by the unsigned integer U1"
  "Push the unsigned remainder U2 and unsigned quotient U3 onto the data stack"
  (let* ((u1 (cell-unsigned (stack-pop data-stack)))
         (d (stack-pop-double data-stack))
         (ud (multiple-value-bind (low high)
                 (double-components d)
               (double-cell-unsigned low high))))
    (if (zerop u1)
        (forth-exception :divide-by-zero)
        (multiple-value-bind (quotient remainder)
            (truncate ud u1)
          (stack-push data-stack (cell-unsigned remainder))
          (stack-push data-stack (cell-unsigned quotient))))))

(define-word multiply-double (:word "U*")
  "( u1 u2 - ud )"
  "Multiply the unsigned integers U1 and U2 and push the resulting unsigned double precision integer UD onto the data stack"
  (let ((u2 (cell-unsigned (stack-pop data-stack)))
        (u1 (cell-unsigned (stack-pop data-stack))))
    (stack-push-double data-stack (* u1 u2))))


;;; 2.2.2 Logical Operations

;;; Single-Precision Operations

(define-word abs (:word "ABS")
  "( n1 - n2 )"
  "Push the absolute value of N1 onto the data stack"
  (stack-push data-stack (cell-signed (abs (stack-pop data-stack)))))

(define-word and (:word "AND")
  "( x1 x2 - x3 )"
  "Return the bitwise logical and of X1 with X2"
  (stack-push data-stack (cell-unsigned (logand (stack-pop data-stack) (stack-pop data-stack)))))

(define-word invert (:word "INVERT")
  "( x1 - x2 )"
  "Invert all bits of X1, giving the logical inverse X2"
  (stack-push data-stack (cell-unsigned (lognot (stack-pop data-stack)))))

(define-word max (:word "MAX")
  "( n1 n2 - n3)"
  "Push the larger of N1 and N2 onto the data stack"
  (stack-push data-stack (cell-signed (max (stack-pop data-stack) (stack-pop data-stack)))))

(define-word min (:word "MIN")
  "( n1 n2 - n3)"
  "Push the smaller of N1 and N2 onto the data stack"
  (stack-push data-stack (cell-signed (min (stack-pop data-stack) (stack-pop data-stack)))))

(define-word negate (:word "NEGATE")
  "( n1 - n2 )"
  "Change the sign of the top of the data stack"
  (stack-push data-stack (cell-signed (- (stack-pop data-stack)))))

(define-word or (:word "OR")
  "( x1 x2 - x3 )"
  "Return the bitwise logical inclusive or of X1 with X2"
  (stack-push data-stack (cell-unsigned (logior (stack-pop data-stack) (stack-pop data-stack)))))

(define-word within (:word "WITHIN")
  "( x1 x2 x3 - flag )"
  "Returns true if X2 <= X1 < X3. X1, X2, and X3 should be either all signed or all unsigned"
  (let ((x3 (stack-pop data-stack))
        (x2 (stack-pop data-stack))
        (x1 (stack-pop data-stack)))
    (if (and (<= x2 x1) (< x1 x3))
        (stack-push data-stack +true+)
        (stack-push data-stack +false+))))

(define-word xor (:word "XOR")
  "( x1 x2 - x3 )"
  "Return the bitwise logical exclusive or of X1 with X2"
  (stack-push data-stack (cell-unsigned (logxor (stack-pop data-stack) (stack-pop data-stack)))))


;;; 2.3.2.1 Variables

(defun push-parameter-as-cell (fs &rest parameters)
  (with-forth-system (fs)
    (stack-push data-stack (first parameters))))

(define-word variable (:word "VARIABLE")
  "VARIABLE <name>"
  "Allocate a cell in data space and create a dictionary entry for <name> which returns the address of that cell"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (allocate-memory memory +cell-size+))
           (word (make-word name #'push-parameter-as-cell :parameters (list address))))
      (add-word (word-lists-compilation-word-list word-lists) word))))

(define-word cvariable (:word "CVARIABLE")
  "CVARIABLE <name>"
  "Allocate space for a character (currently 1 byte) in data space and create a dictionary entry"
  "for <name> which returns the address of that character"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((address (allocate-memory memory +char-size+))
           (word (make-word name #'push-parameter-as-cell :parameters (list address))))
      (add-word (word-lists-compilation-word-list word-lists) word))))


;;; 2.3.2.2 Constants and Values

(define-word constant (:word "CONSTANT")
  "CONSTANT <name>" "( x - )"
  "Create a dictionary entry for <name> which pushes signed integer X on the data stack"
  (let ((name (word files #\Space))
        (value (stack-pop data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'push-parameter-as-cell :parameters (list value))))
      (add-word (word-lists-compilation-word-list word-lists) word))))

(defun push-cell-at-parameter (fs &rest parameters)
  (with-forth-system (fs)
    (stack-push data-stack (memory-cell memory (first parameters)))))

(define-word value (:word "VALUE")
  "VALUE <name>" "( x - )"
  "Allocate a cell in data space, initialize it to X, and create a dictionary entry for <name> which returns"
  "the contents of that cell in data space. To change the value, use TO"
  (let ((name (word files #\Space))
        (value (stack-pop data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory)
    (let* ((address (allocate-memory memory +cell-size+))
           (word (make-word name #'push-cell-at-parameter :parameters (list address :value))))
      (setf (memory-cell memory address) value)
      (add-word (word-lists-compilation-word-list word-lists) word))))


;;; 2.3.3 Arrays and Tables

(define-word create-cell (:word ",")
  "( x - )"
  "Allocate one cell in data space and store x in the cell"
  (let ((value (stack-pop data-stack))
        (address (allocate-memory memory +cell-size+)))
    (setf (memory-cell memory address) value)))
  
(define-word align (:word "ALIGN")
  "( - )"
  "If the data space pointer is not aligned, reserve enough space to align it"
  (align-memory memory))

(define-word aligned (:word "ALIGNED")
  "( addr - a-addr) "
  "Return A-ADDR, the first aligned address greater than or equal to ADDR"
  (let ((addr (stack-pop data-stack)))
    (stack-push data-stack
                (if (zerop (mod addr +cell-size+))
                    addr
                    (+ addr (- +cell-size+ (mod addr +cell-size+)))))))

(define-word allocate (:word "ALLOT")
  "( u - )"
  "Allocate U bytes of data space beginning at the next available location"
  (let ((count (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Byte count to ALLOT can't be negative"))
    (allocate-memory memory count)))

(define-word create-buffer (:word "BUFFER:")
  "BUFFER: <name>" "( n - )"
  "Reserve N bytes of memory and create a dictionary entry for <name> that returns the address of the first byte"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((count (stack-pop data-stack))
           (address (allocate-memory memory count))
           (word (make-word name #'push-parameter-as-cell :parameters (list address))))
      (add-word (word-lists-compilation-word-list word-lists) word))))

(define-word create-char (:word "C,")
  "( char - )"
  "Allocate space for one character in data space and store CHAR"
  (let ((value (stack-pop data-stack))
        (address (allocate-memory memory +char-size+)))
    (setf (memory-char memory address) (extract-char value))))

(define-word cell-incf (:word "CELL+")
  "( a-addr1 - a-addr2 )"
  "Add the size of a cell in bytes to A-ADDR1, giving A-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +cell-size+)))

(define-word cells-size (:word "CELLS")
  "( n1 - n2 )"
  "Return the size in bytes of n1 cells"
  (stack-push data-stack (* (stack-pop data-stack) +cell-size+)))

(define-word char-incf (:word "CHAR+")
  "( a-addr1 - a-addr2 )"
  "Add the size of a character in bytes to A-ADDR1, giving A-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +char-size+)))

(define-word chars-size (:word "CHARS")
  "( n1 - n2 )"
  "Return the size in bytes of n1 characters"
  (stack-push data-stack (* (stack-pop data-stack) +char-size+)))

(define-word create (:word "CREATE")
  "CREATE <name>"
  "Create a dictionary entry for <name> that returns the address of the next available location in data space"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((address (data-space-high-water-mark memory))
           (word (make-word name #'push-parameter-as-cell :parameters (list address))))
      (add-word (word-lists-compilation-word-list word-lists) word))))


;;; 2.3.4 Memory Stack Operations

(define-word write-cell (:word "!")
  "( x a-addr - )"
  "Store the cell X at the address A-ADDR"
  (let ((address (stack-pop data-stack))
        (data (stack-pop data-stack)))
    (setf (memory-cell memory address) data)))

(define-word incf-cell (:word "+!")
  "( n a-addr - )"
  "Add N to the contents of the cell at A-ADDR and store the result back into A-ADDR"
  (let ((address (stack-pop data-stack))
        (n (stack-pop data-stack)))
    (setf (memory-cell memory address) (cell-signed (+ (memory-cell memory address) n)))))

(define-word write-two-cells (:word "2!")
  "( x1 x2 a-addr - )"
  "Store the two cells X1 and X2 in the two cells beginning at the address A-ADDR"
  (let ((address (stack-pop data-stack))
        (data (stack-pop-double data-stack)))
    (setf (memory-double-cell memory address) data)))

(define-word read-two-cells (:word "2@")
  "( a-addr - x1 x2 )"
  "Push the contents of the two cells starting at the address A-ADDR onto the data stack"
  (stack-push-double data-stack (memory-double-cell memory (stack-pop data-stack))))

(define-word read-cell (:word "@")
  "( a-addr - x )"
  "Push the contents of the cell at the address A-ADDR onto the data stack"
  (stack-push data-stack (cell-signed (memory-cell memory (stack-pop data-stack)))))

(define-word write-blanks (:word "BLANK")
  "( c-addr u - )"
  "Set a region of memory, at address C-ADDR and of length U, to ASCII blanks"
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Count to BLANK can't be negative"))
    ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
    (memory-fill memory address count +forth-char-space+)))

(define-word write-char (:word "C!")
  "( char a-addr - )"
  "Store the character CHAR at the address A-ADDR"
  (let ((address (stack-pop data-stack))
        (char (extract-char (stack-pop data-stack))))
    (setf (memory-char memory address) char)))

(define-word incf-char (:word "C+!")
  "( char a-addr - )"
  "Add CHAR to the contents of the character at A-ADDR and store the result back into A-ADDR"
  (let ((address (stack-pop data-stack))
        (char (extract-char (stack-pop data-stack))))
    (setf (memory-char memory address) (extract-char (+ (memory-char memory address) char)))))

(define-word read-char (:word "C@")
  "( a-addr - char )"
  "Push the character at the address A-ADDR onto the data stack"
  (stack-push data-stack (memory-char memory (stack-pop data-stack))))

(define-word erase-memory (:word "ERASE")
  "( c-addr u - )"
  "Set a region of memory, at address C-ADDR and of length U, to zero"
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Count to ERASE can't be negative"))
    (memory-fill memory address count 0)))

(define-word fill-memory (:word "FILL")
  "( c-addr u b - )"
  "Set a region of memory, at address C-ADDR and of length U, to the byte B"
  (let ((byte (ldb (byte 8 0) (stack-pop data-stack)))
        (count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Count to FILL can't be negative"))
    (memory-fill memory address count byte)))

(define-word move-memory (:word "MOVE")
  "( addr1 addr2 u - )"
  "Copy U bytes starting from a source starting at the address ADDR1 to the destination starting at address ADDR2"
  (let ((count (stack-pop data-stack))
        (destination (stack-pop data-stack))
        (source (stack-pop data-stack)))
    (unless (plusp count)
      (forth-exception :invalid-numeric-argument "Count to MOVE can't be negative"))
    (memory-copy memory source destination count)))

(define-word to (:word "TO")
  "TO <name>" "( x - )"
  "Store X in the data space associated with <name> which must have been created with VALUE"
  (let* ((name (word files #\Space))
         (word (lookup word-lists name))
         (value (stack-pop data-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (when (null word)
      (forth-exception :undefined-word "~A is not defined" name))
    (unless (eq (second (word-parameters word)) :value)
      (forth-exception :invalid-name-argument "~A was not created by VALUE"))
    (setf (memory-cell memory (first (word-parameters word))) value)))


;;; 3.1.1 Single Characters

(define-word char (:word "CHAR")
  "CHAR <c>" "( - char )"
  "Parse the next word, usually a single character, and push the ASCII value of its first character onto the data stack"
  (let ((char (word files #\Space)))
    (when (null char)
      (forth-exception :zero-length-name))
    (stack-push data-stack (forth-char (aref char 0)))))

(define-word compile-char (:word "[CHAR]" :immediate? t :compile-only? t :inlineable? nil)
  "[CHAR] <c>" "( - char )"
  "When compiling a definition, parse the next word, usually a single character, and compile the ASCII value"
  "of its first character as a literal which will be pushed onto the data stack when the definition is executed"
  (let ((char (word files #\Space)))
    (when (null char)
      (forth-exception :zero-length-name))
    (push `(stack-push data-stack ,(forth-char (aref char 0))) (word-inline-forms compiling-word))))

(define-word blank (:word "BL")
  "( - char )"
  "Push the character for a blank or space onto the data stack"
  (stack-push data-stack +forth-char-space+))


;;; 3.1.2 Scratch Storage for Strings

(define-word pad (:word "PAD")
  "( - a-addr )"
  "Return the address of a temporary storage area usually used for processing strings"
  (stack-push data-stack (pad-base-address memory)))


;;; 3.1.3 Internal String Format

(define-word decode-counted-string (:word "COUNT")
  "( a-addr1 - a-addr2 u )"
  "Return the size U of the counted string at A-ADDR1 and the address of its text"
  (let ((address (stack-pop data-stack)))
    ;; Length of a counted string is always a single byte regardless of character size
    (stack-push data-stack (1+ address))
    (stack-push data-stack (memory-byte memory address))))


;;; 3.2 Strings in Definitions

(define-word string (:word "S\"" :immediate? t :inlineable? nil)
  "S\" <text>\"" "( - a-addr u )"
  "If interpreted, place TEXT in a temporary buffer and return the address of length of the text"
  "If compiled, compile TEXT into the definition. When executed, place the address and length of the text on the data stack"
  (let* ((text (word files #\"))
         (text-size (* (length text) +char-size+)))
    (case (state fs)
      (:interpreting
       (let ((address (temp-space-base-address memory)))
         (ensure-temp-space-holds memory text-size)
         (multiple-value-bind (forth-memory offset)
             (memory-decode-address memory address)
           (native-into-forth-string text forth-memory offset)
           (stack-push data-stack address)
           (stack-push data-stack text-size))))
      (:compiling
       (let ((address (allocate-memory memory text-size)))
         (multiple-value-bind (forth-memory offset)
             (memory-decode-address memory address)
           (native-into-forth-string text forth-memory offset)
           (push `(stack-push data-stack ,address) (word-inline-forms compiling-word))
           (push `(stack-push data-stack ,text-size) (word-inline-forms compiling-word))))))))

(define-word counted-string (:word "C\"" :immediate? t :compile-only? t :inlineable? nil)
  "C\" <text>\"" "( - a-addr )"
  "Compile TEXT as a counted string into the current definition. When executed, place its address on the data stack"
  (let* ((text (word files #\"))
         (text-size (* (length text) +char-size+))
         ;; Length of a counted string is always a single byte regardless of character size
         (address (allocate-memory memory (1+ text-size))))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (native-into-forth-counted-string text forth-memory offset)
      (push `(stack-push data-stack ,address) (word-inline-forms compiling-word)))))

(define-word type-string (:word ".\"" :immediate? t :inlineable? nil)
  ".\" <text>\""
  "Type TEXT on the console."
  (let ((text (word files #\")))
    (case (state fs)
      (:interpreting
       (write-string text))
      (:compiling
       (push `(write-string ,text) (word-inline-forms compiling-word))))))


;;; 3.3 Strings in Data Structures

(define-word allocate-counted-string (:word ",\"")
  ",\" <text>\""
  "Compile TEXT as a counted string. User is responsible for keeping track of its address in data space"
  (let* ((text (word files #\"))
         (text-size (* (length text) +char-size+))
         ;; Length of a counted string is always a single byte regardless of character size
         (address (allocate-memory memory (1+ text-size))))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (native-into-forth-counted-string text forth-memory offset))))


;;; 3.4 String Management Operations

(define-word here (:word "HERE")
  "( - a-addr )"
  "Push the address of the next available memory location in data space onto the stack"
  (stack-push data-stack (data-space-high-water-mark memory)))


;;; 3.6.1 Input Number Conversion

(define-word parse-number (:word ">NUMBER" :inlineable? nil)
  "( ud1 c-addr u1 - ud2 c-addr u2 )"
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

(define-word simple-parse-number (:word "NUMBER" :inlineable? nil)
  "( c-addr u - n | d )"
  "Attempt to convert the string at C-ADDR of length U into an integer. If the string contains punctuation, return the"
  "double integer D. If the string does not contain punctuation, return the single integer N. If the conversion fails, ABORT"
  (let ((length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of string must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
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
        (memory-decode-address memory address)
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


;;; 3.6.2 Numeric Output

;;; 3.6.2.1 Standard Numeric Output Words

(define-word print-tos (:word ".")
  "( n - )"
  "Display the top cell of the data stack as a signed integer in the current base"
  (let ((value (cell-signed (stack-pop data-stack))))
    (format t "~VR " base value)))

(define-word print-tos-in-field (:word ".R")
  "( n1 +n2 - )"
  "Display N1 right aligned in a field N2 characters wide. If the number of characters required to display N1"
  "is greater than N2, all digits are displayed in a field as wide as necessary with no leading spaces"
  (let ((width (stack-pop data-stack))
        (value (cell-signed (stack-pop data-stack))))
    (unless (plusp width)
      (forth-exception :invalid-numeric-argument "Field width to .R must be positive"))
    (format t "~V,VR" base width value)))

(define-word print-tos-unsigned (:word "U.")
  "( u - )"
  "Display the top cell of the data stack as an unsigned integer in the current base"
  (let ((value (cell-unsigned (stack-pop data-stack))))
    (format t "~VR " base value)))

(define-word print-tos-unsigned-in-field (:word "U.R")
  "( u +n - )"
  "Display U right aligned in a field N characters wide. If the number of characters required to display U"
  "is greater than N, all digits are displayed in a field as wide as necessary with no leading spaces"
  (let ((width (stack-pop data-stack))
        (value (cell-unsigned (stack-pop data-stack))))
    (unless (plusp width)
      (forth-exception :invalid-numeric-argument "Field width to U.R must be positive"))
    (format t "~V,VR" base width value)))


;;; 4.2 Comparison and Testing Operations

(define-word minusp (:word "0<")
  " ( n - flag )"
  "Return true if N is less than zero"
  (stack-push data-stack (if (minusp (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word not-zerop (:word "0<>")
  " ( n - flag )"
  "Return true if N is not equal to zero"
  (stack-push data-stack (if (zerop (cell-signed (stack-pop data-stack))) +false+ +true+)))

(define-word zerop (:word "0=")
  " ( n - flag )"
  "Return true if N is equal to zero"
  (stack-push data-stack (if (zerop (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word plusp (:word "0>")
  " ( n - flag )"
  "Return true if N is greater than zero"
  (stack-push data-stack (if (plusp (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word less-than (:word "<")
  " ( n1 n2 - flag )"
  "Return true if N1 is less than N2"
  ;; As the first value popped off the stack is N2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (>= (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word not-equal (:word "<>")
  " ( n1 n2 - flag )"
  "Return true if N1 is not equal to N2"
  (stack-push data-stack (if (/= (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word equal (:word "=")
  " ( n1 n2 - flag )"
  "Return true if N1 is equal to N2"
  (stack-push data-stack (if (= (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word greater-than (:word ">")
  " ( n1 n2 - flag )"
  "Return true if N1 is greater than N2"
  ;; As the first value popped off the stack is N2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (< (cell-signed (stack-pop data-stack)) (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word false (:word "FALSE")
  "( - flag )"
  "Return a FLAG that is false"
  (stack-push data-stack +false+))

(define-word not (:word "NOT")
  "( n - flag )"
  "Identical to 0=, used for program clarity to reverse the results of a previous test"
  (stack-push data-stack (if (zerop (cell-signed (stack-pop data-stack))) +true+ +false+)))

(define-word true (:word "TRUE")
  "( - flag )"
  "Return a FLAG that is true"
  (stack-push data-stack +true+))

(define-word less-than-unsigned (:word "U<")
  " ( u1 u2 - flag )"
  "Return true if U1 is less than U2"
  ;; As the first value popped off the stack is U2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (>= (cell-unsigned (stack-pop data-stack)) (cell-unsigned (stack-pop data-stack)))
                             +true+ +false+)))

(define-word greater-than-unsigned (:word "U>")
  " ( u1 u2 - flag )"
  "Return true if U1 is greater than U2"
  ;; As the first value popped off the stack is U2, we'll reverse the sense of the test to get the proper answer
  (stack-push data-stack (if (< (cell-unsigned (stack-pop data-stack)) (cell-unsigned (stack-pop data-stack))) +true+ +false+)))


;;; 4.3 Conditionals

;;; IF
;;; THEN
;;; ELSE


;;; 5.3 Exception Handling

(define-word abort (:word "ABORT")
  "(S: i*x - ) (R: j*x - )"
  "Empty the data and return stacks, reset the input source to the console (SOURCE-ID 0), and restart the interpreter"
  (forth-exception :abort))

;;; Marked as IMMEDIATE so we can grab the message at compile-time and generate the correct code sequence
(define-word abort-with-message (:word "ABORT\"" :immediate? t :compile-only? t)
  "ABORT\" <message>\""
  "(S: i*x x1 - | i*x ) (R: j*x - | j*x )"
  "At compile time, parse MESSAGE from the input buffer. At runtime, if X1 is true (i.e., non-zero), display"
  "the message and perform the actions of ABORT"
  (let ((message (word files #\")))
    (push `(when (truep (stack-pop data-stack))
             (forth-exception :abort\" "~@[In ~A: ~]~A" ,(word-name compiling-word) ,message))
          (word-inline-forms compiling-word))))


;;; 5.4.2 Terminal Output

(define-word write-char (:word "EMIT")
  " ( char - )"
  "Type the character CHAR on the terminal"
  (write-char (native-char (extract-char (stack-pop data-stack)))))


(define-word write-string (:word "TYPE")
  " ( a-addr u - )"
  "Type the string at A-ADDR of length U on the terminal"
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to TYPE can't be negative"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (write-string (forth-string-to-native forth-memory offset count)))))


;;; 5.4.3 Support of Special Terminal Features

(define-word terpri (:word "CR")
  "Cause subsequent output to the terminal to appear on a new line"
  (terpri))

(define-word space (:word "SPACE")
  "Write a space to the terminal"
  (write-char #\Space))

(defconstant +spaces+ "                                ")

(define-word spaces (:word "SPACES")
  "( u - )"
  "Write U spaces to the terminal"
  (let ((count (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to SPACES can't be negative"))
    (loop with n-spaces = (length +spaces+)
          for n = count then (- n n-spaces)
          while (plusp n)
          do (write-string (subseq +spaces+ 0 (min n n-spaces))))))


;;; 6.1.2 Input Source Management

(define-word evaluate-string (:word "EVALUATE")
  "( i*x c-addr u – j*x )"
  "Save the current input source specification. Store minus-one (-1) in SOURCE-ID. Make the string described by c-ADDR and U"
  "both the input source and input buffer, set >IN to zero, and interpret. When the parse area is empty, restore the prior"
  "input source specification. Other stack effects are due to the words EVALUATEd."
  (let ((count (stack-pop data-stack))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to EVALUATE can't be negative"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (let ((string (forth-string-to-native forth-memory offset count)))
        (source-push files :evaluate string)))))

(define-word reset-interpreter (:word "QUIT")
  "(S: i*x - ) (R: j*x - )"
  "Empty the data and return stacks, reset the input source to the terminal, restart the interpreter loop."
  "Do not display a message."
  (forth-exception :quit))

(define-word refill (:word "REFILL")
  "( - flag )"
  "Attempt to fill the input buffer from the current input source, returning true if successful"
  (stack-push data-stack (if (refill files) +true+ +false+)))

;;; RESTORE-INPUT
;;; SAVE-INPUT

(define-word source (:word "SOURCE")
  "( - c-addr u )"
  "Return the address C-ADDR and size U of the input buffer"
  (multiple-value-bind (address count)
      (access-source-buffer files)
    (stack-push data-stack address)
    (stack-push data-stack count)))


;;; 6.1.3 Parsing Text in the Input Stream

;;; PARSE
;;; WORD


;;; 6.2.2 Colon Definitions

;; Mark this word as IMMEDIATE so we can catch recursive compilation, probably the result of a missing ";"
(define-word start-definition (:word ":" :immediate? t)
  ": <name>"
  "Create a definition for <name>. Enter compilation state and start compiling the definition."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (begin-compilation fs name)))

;;; :NONAME

(define-word finish-definition (:word ";" :immediate? t :compile-only? t)
  "Complete the current definition and make it available for use. Align the data space pointer to a cell boundary"
  (finish-compilation fs)
  (align-memory memory))

(define-word recurse (:word "RECURSE" :immediate? t :compile-only? t :inlineable? nil)
  "Append the execution behavior of the current definition to the current definition, so that it calls itself recursively"
  (push `(forth-call fs ,compiling-word) (word-inline-forms compiling-word)))


;;; 6.3.1 The Forth Compiler

;;; COMPILE,

(define-state-word state)

(define-word interpret (:word "[" :immediate? t :compile-only? t)
  "Temporarily switch from compiling a definition to interpreting words"
  (setf compiling-paused? t)
  (setf (state fs) :interpreting))

(define-word compile (:word "]")
  "Switch back to compiling a definition after using ']'"
  (unless (shiftf compiling-paused? nil)
    (forth-exception :not-compiling "Can't resume compiling when nothing's being compiled"))
  (setf (state fs) :compiling))


;;; 6.3.2 Literals and Constants

(define-word literal (:word "LITERAL" :immediate? t :compile-only? t)
  "( x - )"
  "Compile X into the current definition. When executed, push X onto the data stack"
  (let ((value (stack-pop data-stack)))
    (push `(stack-push data-stack ,value) (word-inline-forms compiling-word))))


;;; 6.6.2 Managing Word Lists

(define-state-word context)

(define-state-word current)

(defun replace-top-of-search-order-with-parameter (fs &rest parameters)
  (with-forth-system (fs)
    (replace-top-of-search-order word-lists (first parameters))))

(define-word create-word-list (:word "VOCABULARY")
  "VOCABULARY <name>"
  "Create an empty word list and define NAME to to replace the first word list in the sarch order with this new list"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((dict (vocabulary word-lists name))
           (word (make-word name #'replace-top-of-search-order-with-parameter :parameters (list dict))))
      (add-word (word-lists-compilation-word-list word-lists) word))))
