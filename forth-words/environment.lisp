(in-package #:forth)

(defvar *environment-queries* (make-hash-table :test #'equalp))

(defstruct query
  name
  result)

(defmacro define-query (name &body body)
  (let ((thunk `(lambda (fs)
                  (with-forth-system (fs)
                    ,@body
                    (stack-push data-stack +true+)))))
    `(setf (gethash ,name *environment-queries*) (make-query :name ,name :result (compile nil ,thunk)))))

(define-word environment-query (:word "ENVIRONMENT?")
  "( c-addr u – false | i*x true )"
  "c-ADDR is the address of a character string and U is the string’s character count. U may have a value in the range"
  "from zero to an implementation-defined maximum which shall not be less than 31. The character string should contain"
  "a keyword from 3.2.6 Environmental queries or the optional word sets to be checked for correspondence with an attribute"
  "of the present environment. If the system treats the attribute as unknown, the returned FLAG is false;"
  "otherwise, the FLAG is true and the I * X returned is of the type specified in the table for the attribute queried"
  (let ((count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to ENVIRONMENT? can't be negative"))
    (multiple-value-bind (region offset)
        (memory-decode-address memory address)
      (let* ((query-string (forth-string-to-native region offset count))
             (query (gethash query-string *environment-queries*)))
        (if query
            (funcall (query-result query) fs)
            (stack-push data-stack +false+))))))


;;; Core queries defined in the Forth 2012 specification

(define-query "/COUNTED-STRING"
  (stack-push data-stack +longest-counted-string+))

(define-query "/HOLD"
  (stack-push data-stack +pictured-buffer-size+))

(define-query "/PAD"
  (stack-push data-stack +pad-space-size+))

(define-query "ADDRESS-UNIT-BITS"
  (stack-push data-stack 8))

(define-query "FLOORED"
  ;; CL-Forth uses symmetric division
  (stack-push data-stack +false+))

(define-query "MAX-CHAR"
  ;; Claim CL-Forth only supports the ASCII character set
  (stack-push data-stack (char-code #\Rubout)))

(define-query "MAX-D"
  (stack-push-double data-stack +most-positive-double-cell+))

(define-query "MAX-N"
  (stack-push data-stack +most-positive-single-cell+))

(define-query "MAX-U"
  (stack-push data-stack +maximum-unsigned-single-cell+))

(define-query "MAX-UD"
  (stack-push-double data-stack +maximum-unsigned-double-cell+))

(define-query "RETURN-STACK-CELLS"
  (stack-push data-stack (stack-size return-stack)))

(define-query "STACK-CELLS"
  (stack-push data-stack (stack-size data-stack)))


;;; Floating-Point word set queries defined in the Forth 2012 specification

(define-query "FLOATING-STACK"
  (stack-push data-stack (stack-size float-stack)))

(define-query "MAX-FLOAT"
  (stack-push float-stack +most-positive-native-float+))


;;; Locals word set queries defined in the Forth 2012 specification
;;;  NOTE: CL-Forth doesn't yet support locals

#||
(define-query "#LOCALS"
  16)
||#


;;; Search-Order word set queries defined in the Forth 2012 specification

(define-query "WORDLISTS"
  ;; CL-Forth doesn't actually enforce a limit on the size of the search order
  (stack-push data-stack 128))


;;; Extended-Character word set queries defined in the Forth 2012 specification
;;;  NOTE: CL-Forth doesn't yet support extended characters

#||
(defconstant +xchar-encoding-name+ "UTF-32")

(define-query "XCHAR-ENCODING"
  (let ((address (allocate-memory memory (length +xchar-encoding-name+))))
    (multiple-value-bind (region offset)
        (memory-decode-address memory address)
      (native-into-forth-string +xchar-encoding-name+ region offset))
    (stack-push data-stack address)
    (stack-push data-stack (length +xchar-encoding-name+))))

(define-query "MAX-XCHAR"
  (stack-push data-stack char-code-limit))

(define-query "XCHAR-MAXMEM"
  (stack-push data-stack 4))
||#


;;; Queries for the presence of Forth 1994 word sets -- Marked as obsolescent in Forth 2012

;;; CL-Forth does not implement KEY
(define-query "CORE"
  (stack-push data-stack +true+))

;;; CL-Forth does not implement #TIB, CONVERT, EXPECT, QUERY, SPAN, and TIB
(define-query "CORE-EXT"
  (stack-push data-stack +true+))

;;; CL-Forth does not implement the Block word set
(define-query "BLOCK"
  (stack-push data-stack +false+))

;;; CL-Forth does not implement the Block word set
(define-query "BLOCK-EXT"
  (stack-push data-stack +false+))

(define-query "DOUBLE"
  (stack-push data-stack +true+))

(define-query "DOUBLE-EXT"
  (stack-push data-stack +true+))

(define-query "EXCEPTION"
  (stack-push data-stack +true+))

(define-query "EXCEPTION-EXT"
  (stack-push data-stack +true+))

(define-query "FACILITY"
  (stack-push data-stack +false+))

;;; CL-Forth does not implement EKEY, EKEY>CHAR, EKEY>FKEY, EKEY?, EMIT?, K-ALT-MASK, K-CTRL-MASK, K-DELETE,
;;;  K-DOWN, K-END, K-F1, K-F10, K-F11, K-F12, K-F2, K-F3, K-F4, K-F5, K-F6, K-F7, K-K8, K-F9, K-HOME,
;;;  K-INSERT, K-LEFT, K-NEXT, K-PRIOR, K-RIGHT, K-SHIFT-MASK, and K-UP
(define-query "FACILITY-EXT"
  (stack-push data-stack +false+))

(define-query "FILE"
  (stack-push data-stack +true+))

(define-query "FILE-EXT"
  (stack-push data-stack +true+))

(define-query "FLOATING"
  (stack-push data-stack +true+))

(define-query "FLOATING-EXT"
  (stack-push data-stack +true+))

;;; CL-Forth does not yet implement the Locals word set
(define-query "LOCALS"
  (stack-push data-stack +false+))

;;; CL-Forth does not yet implement the Locals word set
(define-query "LOCALS-EXT"
  (stack-push data-stack +false+))

(define-query "MEMORY-ALLOC"
  (stack-push data-stack +false+))

(define-query "MEMORY-ALLOC-EXT"
  (stack-push data-stack +false+))

(define-query "TOOLS"
  (stack-push data-stack +true+))

(define-query "TOOLS-EXT"
  (stack-push data-stack +true+))

(define-query "SEARCH-ORDER"
  (stack-push data-stack +true+))

(define-query "SEARCH-ORDER-EXT"
  (stack-push data-stack +true+))

(define-query "STRING"
  (stack-push data-stack +true+))

(define-query "STRING-EXT"
  (stack-push data-stack +true+))
