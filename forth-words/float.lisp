(in-package #:forth)

;;; CL-Forth uses double precision floating point as its internal representation of float values

;;; Floating-Point words as defined in Section 12 of the Forth 2012 specification

(defmacro with-float-exceptions ((&key zero-divide-is-out-of-range? handler) &body body)
  `(handler-case
       (progn ,@body)
     (arithmetic-error (c)
       (declare (ignorable c))
       ,(if handler
            handler
            `(typecase c
               (division-by-zero
                ,(if zero-divide-is-out-of-range?
                     `(forth-exception :floating-out-of-range)
                     `(forth-exception :floating-divide-by-zero)))
               (floating-point-overflow
                (forth-exception :floating-out-of-range))
               (floating-point-underflow
                (forth-exception :float-underflow))
               (floating-point-inexact
                (forth-exception :loss-of-precision))
               (otherwise
                (forth-exception :float-unknown-fault)))))))

(define-word string-to-float (:word ">FLOAT")
  "( c-addr u – true | false ) (F: – r | )"
  "An attempt is made to convert the string specified by C-ADDR and U to internal floating-point representation."
  "If the string represents a valid floating-point number, its value R and TRUE are returned. If the string does"
  "not represent a valid floating-point number only FALSE is returned"
  (let ((length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "String length must be positive"))
    (multiple-value-bind (region offset)
        (memory-decode-address memory address length)
      (let ((string (forth-string-to-native region offset length)))
        ;; Remove leading and trailing spaces
        (setf string (string-trim '(#\Space) string)
              length (length string))
        ;; Forth 2012 expects "-35+2" to be converted to -3500.0 whereas Lisp will interpret that string
        ;; as a symbol. So, we'll explicitly check for this situation and insert an "E" into the string
        ;; before passing it to Lisp's reader.
        (let ((position (position-if #'(lambda (ch) (or (char-equal ch #\+) (char-equal ch #\-))) string :from-end t)))
          (when (and position
                     (plusp position)
                     (let ((ch (aref string (1- position))))
                       (or (digit-char-p ch) (char-equal ch #\.))))
            (setf string (concatenate 'string (subseq string 0 position) "E" (subseq string position)))
            (incf length)))
        ;; If the last character is an exponent marker (E/e/D/d) or a sign (+/-), append a "0" so that
        ;; the Lisp reader will attempt to convert the string to a number
        (when (and (plusp length)
                   (member (aref string (1- (length string))) '(#\E #\D #\+ #\-) :test #'char-equal))
          (setf string (concatenate 'string string "0"))
          (incf length))
        (with-float-exceptions ()
          (with-standard-io-syntax
            (with-native-float-format ()
              (let ((*read-eval* nil))
                (multiple-value-bind (object position)
                    (read-from-string string nil nil)
                  (declare (ignore position))
                  (cond ((numberp object)
                         ;; String is a number
                         (stack-push data-stack +true+)
                         (stack-push float-stack (native-float object)))
                        ((null object)
                         ;; String was just whitespace -- Forth 2012 specification suggests that we
                         ;; return zero in this case. (A future standard may require this behavior) 
                         (stack-push data-stack +true+)
                         (stack-push float-stack (native-float 0)))
                        (t
                         ;; Not just a number
                         (stack-push data-stack +false+))))))))))))

(define-word double-to-float (:word "D>F")
  "( d – ) (F: – r)"
  "R is the floating-point equivalent of D"
  (let* ((double (stack-pop-double data-stack))
         (float (native-float double)))
    (unless (= double float)
      (forth-exception :loss-of-precision))
    (stack-push float-stack float)))

(define-word float-store (:word "F!")
  "( f-addr – ) (F: r – )"
  "Store R at F-ADDR"
  (let ((float (stack-pop float-stack))
        (address (stack-pop data-stack)))
    (setf (memory-native-float memory address) float)))

(define-word float-multiply (:word "F*")
  "(F: r1 r2 – r3 )"
  "Multiply R1 by R2 giving R3"
  (with-float-exceptions ()
    (stack-push float-stack (* (stack-pop float-stack) (stack-pop float-stack)))))

(define-word float-multiply (:word "F+")
  "(F: r1 r2 – r3 )"
  "Add R1 to R2 giving the sum R3"
  (with-float-exceptions ()
    (stack-push float-stack (+ (stack-pop float-stack) (stack-pop float-stack)))))

(define-word float-subtract (:word "F-")
  "(F: r1 r2 – r3 )"
  "Subtract R2 from R1 giving R3"
  (with-float-exceptions ()
    (let ((r2 (stack-pop float-stack))
          (r1 (stack-pop float-stack)))
      (stack-push float-stack (- r1 r2)))))

(define-word float-divide (:word "F/")
  "(F: r1 r2 – r3 )"
  "Divide R1 by R2 giving the quotient R3"
  (with-float-exceptions ()
    (let ((r2 (stack-pop float-stack))
          (r1 (stack-pop float-stack)))
      (if (zerop r2)
          (forth-exception :floating-divide-by-zero)
          (stack-push float-stack (/ r1 r2))))))

(define-word float-minusp (:word "F0<")
  "( – flag ) (F: r – )"
  "FLAG is true if and only if R is less than zero"
  (if (minusp (stack-pop float-stack))
      (stack-push data-stack +true+)
      (stack-push data-stack +false+)))

(define-word float-zerop (:word "F0=")
  "( – flag ) (F: r – )"
  "FLAG is true if and only if R is equal to zero"
  (if (zerop (stack-pop float-stack))
      (stack-push data-stack +true+)
      (stack-push data-stack +false+)))

(define-word float-less-than (:word "F<")
  "( – flag ) (F: r1 r2 – )"
  "FLAG is true if and only if R1 is less than R2"
  (let ((r2 (stack-pop float-stack))
        (r1 (stack-pop float-stack)))
    (if (< r1 r2)
        (stack-push data-stack +true+)
        (stack-push data-stack +false+))))

(define-word float-to-double (:word "F>D")
  "( – d ) (F: r – )"
  "D is the double-cell signed-integer equivalent of the integer portion of R, rounded towards zero."
  "The fractional portion of R is discarded."
  (let ((double (truncate (stack-pop float-stack))))
    (if (<= +most-negative-double-cell+ double +most-positive-double-cell+)
        (stack-push-double data-stack double)
        (forth-exception :floating-out-of-range))))

(define-word float-fetch (:word "F@")
  "( f-addr – ) (F: – r)"
  "R is the value stored at F-ADDR"
  (stack-push float-stack (memory-native-float memory (stack-pop data-stack))))

(define-word float-align (:word "FALIGN")
  "If the data-space pointer is not float aligned, reserve enough data space to make it so"
  (align-memory +native-float-cell-size+))

(define-word float-aligned (:word "FALIGNED")
  "( addr – f-addr )"
  "'F-ADDR is the first float-aligned address greater than or equal to ADDR"
  (let ((addr (stack-pop data-stack)))
    (stack-push data-stack
                (if (zerop (mod addr +native-float-cell-size+))
                    addr
                    (+ addr (- +cell-size+ (mod addr +native-float-cell-size+)))))))

(define-word f-constant (:word "FCONSTANT")
  "FCONSTANT <name>" "(F: r – )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Create a definition for NAME with the execution semantics"
  "defined below. NAME is referred to as an \"f-constant\""
  "NAME Execution: ( – ) ( F: – r )"
  "Place R on the floating-point stack"
  (let ((name (word files #\Space))
        (value (stack-pop float-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'push-parameter-as-float :parameters (list value))))
      (add-and-register-word fs word (data-space-high-water-mark memory)))))

(define-word float-stack-depth (:word "FDEPTH")
  "( – +n )"
  "+N is the number of values contained on the floating-point stack"
  (stack-push data-stack (stack-depth float-stack)))

(define-word float-stack-drop (:word "FDROP")
  "(F: r – )"
  "Remove R from the floating-point stack"
  (stack-drop float-stack))

(define-word float-stack-dup (:word "FDUP")
  "(F: r - r r )"
  "Duplicate R"
  (stack-dup float-stack))

(define-word float-literal (:word "FLITERAL" :immediate? t :compile-only? t)
  "Compilation: (F: r – )"
  "Append the run-time semantics given below to the current definition"
  "Run-time: (F: - r)"
  "Place R on the floating-point stack"
  (let ((value (stack-pop float-stack)))
    (add-to-definition fs
      `(stack-push float-stack ,value))))

(define-word float-plus (:word "FLOAT+")
  "( f-addr1 – f-addr2 )"
  "Add the size in address units of a floating-point number to F-ADDR1, giving f-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +native-float-cell-size+)))

(define-word floats (:word "FLOATS")
  "( n1 – n2 )"
  "N2 is the size in address units of N1 floating-point numbers"
  (stack-push data-stack (* (stack-pop data-stack) +native-float-cell-size+)))

(define-word floor (:word "FLOOR")
  "(F: r1 – r2 )"
  "Round R1 to an integral value using the \"round toward negative infinity\" rule, giving R2"
  (stack-push float-stack (native-float (floor (stack-pop float-stack)))))

(define-word float-max (:word "FMAX")
  "(F: r1 r2 – r3 )"
  "R3 is the greater of R1 and R2"
  (stack-push float-stack (max (stack-pop float-stack) (stack-pop float-stack))))

(define-word float-min (:word "FMIN")
  "(F: r1 r2 – r3 )"
  "R3 is the lesser of R1 and R2"
  (stack-push float-stack (min (stack-pop float-stack) (stack-pop float-stack))))

(define-word float-negate (:word "FNEGATE")
  "(F: r1 - r2 )"
  "R2 is the negation of R1"
  (stack-push float-stack (- (stack-pop float-stack))))

(define-word float-stack-over (:word "FOVER")
  "(F: r1 r2 – r1 r2 r1 )"
  "Place a copy of R1 on top of the floating-point stack"
  (stack-over float-stack))

(define-word float-stack-rotate (:word "FROT")
  "(F: r1 r2 r3 – r2 r3 r1 )"
  "Rotate the top three floating-point stack entries"
  (stack-rot float-stack))

(define-word float-round (:word "FROUND")
  "(F: r1 – r2 )"
  "Round R1 to an integral value using the \"round to nearest\" rule, giving R2"
  (stack-push float-stack (native-float (round (stack-pop float-stack)))))

(define-word float-stack-swap (:word "FSWAP")
  "(F: r1 r2 – r2 r1 )"
  "Exchange the top two floating-point stack items"
  (stack-swap float-stack))

(define-word float-variable (:word "FVARIABLE")
  "FVARIABLE <name>"
  "Skip leading space delimiters. Parse NAME delimited by a space. Create a definition for NAME with the execution semantics"
  "defined below. Reserve 1 FLOATS address units of data space at a float-aligned address."
  "NAME is referred to as an \"f-variable\""
  "NAME Execution: ( – f-addr )"
  "F-ADDR is the address of the data space reserved by FVARIABLE when it created NAME."
  "A program is responsible for initializing the contents of the reserved space"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory +native-float-cell-size+)
    (let* ((address (allocate-memory memory +native-float-cell-size+))
           (word (make-word name #'push-parameter-as-cell :parameters (list address) :creating-word? t)))
      (add-and-register-word fs word address))))

(define-word float-representation (:word "REPRESENT")
  "( c-addr u – n flag1 flag2 ) (F: r – )"
  "At C-ADDR, place the character-string external representation of the significand of the floating-point number R."
  "Return the decimal-base exponent as N, the sign as FLAG1 and \"valid result\" as FLAG2. The character string shall"
  "consist of the U most significant digits of the significand represented as a decimal fraction with the implied decimal"
  "point to the left of the first digit, and the first digit zero only if all digits are zero. The significand is rounded"
  "to U digits following the \"round to nearest\" rule; N is adjusted, if necessary, to correspond to the rounded"
  "magnitude of the significand. If FLAG2 is true then R was in the implementation-defined range of floating-point numbers."
  "If FLAG1 is true then R is negative"
  (unless (= base 10)
    (forth-exception :invalid-floating-base))
  (with-native-float-format ()
    (with-float-exceptions (:handler (progn
                                       (stack-push data-stack +false+)
                                       (stack-push data-stack +false+)
                                       (stack-push data-stack 0)))
      (let ((length (cell-signed (stack-pop data-stack)))
            (address (stack-pop data-stack)))
        (unless (plusp length)
          (forth-exception :invalid-numeric-argument "String length must be positive"))
        (multiple-value-bind (region offset)
            (memory-decode-address memory address length)
          (let* ((r (stack-pop float-stack))
                 (exponent (1+ (cond ((plusp r) (floor (log (rationalize r) 10)))
                                     ((zerop r) 0)
                                     ((minusp r) (floor (log (abs (rationalize r)) 10))))))
                 (representation (format nil "~V,V,VF" (1+ length) length (- exponent) (abs r))))
            (stack-push data-stack +true+)
            (stack-push data-stack (if (minusp r) +true+ +false+))
            (stack-push data-stack exponent)
            (native-into-forth-string (subseq representation 1) region offset)))))))

        
;;; Floating-Point extension  words as defined in Section 12 of the Forth 2012 specification

(define-word dfloat-store (:word "DF!")
  "( df-addr – ) (F: r – )"
  "Store the floating-point number R as a 64-bit IEEE double-precision number at DF-ADDR"
  (with-float-exceptions ()
    (let ((float (>double-float (stack-pop float-stack)))
          (address (stack-pop data-stack)))
      (setf (memory-double-float memory address) float))))

(define-word dfloat-fetch (:word "DF@")
  "( df-addr – ) (F: – r)"
  "Fetch the 64-bit IEEE double-precision number stored at DF-ADDR to the floating-point stack as R in"
  "the internal representation"
  (with-float-exceptions ()
    (stack-push float-stack (native-float (memory-double-float memory (stack-pop data-stack))))))

(define-word dfloat-align (:word "DFALIGN")
  "If the data-space pointer is not double-float aligned, reserve enough data space to make it so"
  (align-memory +double-float-cell-size+))

(define-word dfloat-aligned (:word "DFALIGNED")
  "( addr – sf-addr )"
  "SF-ADDR is the first double-float-aligned address greater than or equal to ADDR"
  (let ((addr (stack-pop data-stack)))
    (stack-push data-stack
                (if (zerop (mod addr +double-float-cell-size+))
                    addr
                    (+ addr (- +double-float-cell-size+ (mod addr +double-float-cell-size+)))))))

(define-word dffield (:word "DFFIELD:")
  "DFFIELD: <name>" "( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first double-float aligned value greater than"
  "or equal to N1. N2 = OFFSET + 1 double-float. Create a definition for NAME with the execution semantics given below."
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((original-offset (stack-pop data-stack))
           (offset (if (zerop (mod original-offset +double-float-cell-size+))
                       original-offset
                       (+ original-offset (- +double-float-cell-size+ (mod original-offset +double-float-cell-size+)))))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (list offset))))
      (push word (fs-fields (stack-cell data-stack 0)))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset +double-float-cell-size+)))))

(define-word dfloat-plus (:word "DFLOAT+")
  "( df-addr1 – df-addr2 )"
  "Add the size in address units of a 64-bit IEEE double-precision number to DF-ADDR1, giving DF-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +double-float-cell-size+)))

(define-word dfloats (:word "DFLOATS")
  "( n1 – n2 )"
  "N2 is the size in address units of N1 64-bit IEEE double-precision numbers"
  (stack-push data-stack (* (stack-pop data-stack) +double-float-cell-size+)))

(define-word float-power (:word "F**")
  "(F: r1 r2 – r3 )"
  "Raise R1 to the power R2, giving the product R3"
  (with-float-exceptions ()
    (let ((r2 (stack-pop float-stack))
          (r1 (stack-pop float-stack)))
      (stack-push float-stack (expt r1 r2)))))

(define-word float-display-fixed (:word "F.")
  "(F: r – )"
  "Display, with a trailing space, the top number on the floating-point stack using fixed-point notation"
  (unless (= base 10)
    (forth-exception :invalid-floating-base))
  (with-native-float-format ()
    (let* ((r (stack-pop float-stack))
           (leading-digits (cond ((plusp r) (1+ (floor (log (rationalize r) 10))))
                                 ((zerop r) 1)
                                 ((minusp r) (1+ (floor (log (abs (rationalize r)) 10)))))))
      ;; As a special case, if the number has no fractional part, suppress all digits after the decimal point
      (when (zerop (nth-value 1 (truncate r)))
        (setf leading-digits float-precision))
      (format t "~,VF " (max (- float-precision leading-digits) 0) r))))

(define-word float-to-single (:word "F>S")
  "( – n ) (F: r – )"
  "N is the single-cell signed-integer equivalent of the integer portion of R, rounded towards zero."
  "The fractional portion of R is discarded."
  (let ((single (truncate (stack-pop float-stack))))
    (if (<= +most-negative-single-cell+ single +most-positive-single-cell+)
        (stack-push data-stack single)
        (forth-exception :floating-out-of-range))))

(define-word float-absolute-value (:word "FABS")
  "(F: r1 – r2 )"
  "R2 is the absolute value of R1"
  (stack-push float-stack (abs (stack-pop float-stack))))

(define-word float-acos (:word "FACOS")
  "(F: r1 – r2 )"
  "R2 is the principal radian angle whose cosine is R1"
  (let ((r1 (stack-pop float-stack)))
    ;; Lisp will produce a complex result when |R1| > 1.0 which Forth doesn't support
    (if (<= (abs r1) (native-float 1.0))
        (stack-push float-stack (acos r1))
        (forth-exception :floating-out-of-range))))

(define-word float-acosh (:word "FACOSH")
  "(F: r1 – r2 )"
  "R2 is the floating-point value whose hyperbolic cosine is R1"
  (let ((r1 (stack-pop float-stack)))
    ;; Lisp will produce a complex result when R1 < 1.0 which Forth doesn't support
    (if (>= r1 (native-float 1.0))
        (stack-push float-stack (acosh r1))
        (forth-exception :floating-out-of-range))))

(define-word float-alog (:word "ALOG")
  "(F: r1 – r2 )"
  "Raise ten to the power R1, giving R2"
  (with-float-exceptions ()
    (stack-push float-stack (expt (native-float 10.0) (stack-pop float-stack)))))

(define-word float-asin (:word "FASIN")
  "(F: r1 – r2 )"
  "R2 is the principal radian angle whose sine is R1"
  (let ((r1 (stack-pop float-stack)))
    ;; Lisp will produce a complex result when |R1| > 1.0 which Forth doesn't support
    (if (<= (abs r1) (native-float 1.0))
        (stack-push float-stack (asin r1))
        (forth-exception :floating-out-of-range))))

(define-word float-asinh (:word "FASINH")
  "(F: r1 – r2 )"
  "R2 is the floating-point value whose hyperbolic sine is R1"
  (stack-push float-stack (asinh (stack-pop float-stack))))

(define-word float-atan (:word "FATAN")
  "(F: r1 – r2 )"
  "R2 is the principal radian angle whose tangent is R1"
  (with-float-exceptions ()
    (stack-push float-stack (atan (stack-pop float-stack)))))

(define-word float-atan2 (:word "FATAN2")
  "(F: r1 r2 - r3)"
  "R3 is the principal radian angle (between -π and π) whose tangent is R1/R2. A system that returns false for"
  "\"-0E 0E 0E F~\" shall return a value (approximating) −π when r1 = 0E and r2 is negative"
  (with-float-exceptions ()
    (let ((r2 (stack-pop float-stack))
          (r1 (stack-pop float-stack)))
      (stack-push float-stack (atan r1 r2)))))

(define-word float-atanh (:word "FATANH")
  "(F: r1 – r2 )"
  "R2 is the floating-point value whose hyperbolic tangent is R1"
  (with-float-exceptions (:zero-divide-is-out-of-range? t)
    (let ((r1 (stack-pop float-stack)))
    ;; Lisp will produce a complex result when |R1| > 1.0 which Forth doesn't support
    (if (<= (abs r1) (native-float 1.0))
        (stack-push float-stack (atanh r1))
        (forth-exception :floating-out-of-range)))))

(define-word float-cos (:word "FCOS")
  "(F: r1 – r2 )"
  "R2 is the cosine of the radian angle R1"
  (stack-push float-stack (cos (stack-pop float-stack))))

(define-word float-cosh (:word "FCOSH")
  "(F: r1 – r2 )"
  "R2 is the hyperbolic cosine of R1"
  (with-float-exceptions ()
    (stack-push float-stack (cosh (stack-pop float-stack)))))

(define-word float-display-engineering (:word "FE.")
  "(F: r – )"
  "Display, with a trailing space, the top number on the floating-point stack using engineering notation, where the"
  "significand is greater than or equal to 1.0 and less than 1000.0 and the decimal exponent is a multiple of three"
  (unless (= base 10)
    (forth-exception :invalid-floating-base))
  (with-native-float-format ()
    (let* ((r (stack-pop float-stack))
           (exponent (cond ((plusp r) (floor (log (rationalize r) 10)))
                           ((zerop r) 0)
                           ((minusp r) (floor (log (abs (rationalize r)) 10))))))
      (format t "~,V,,VE " (1- float-precision) (1+ (mod exponent 3)) r))))

(define-word float-exp (:word "FEXP")
  "(F: r1 – r2 )"
  "Raise e to the power R1, giving R2"
  (with-float-exceptions ()
    (stack-push float-stack (exp (stack-pop float-stack)))))

(define-word float-exp-minus-one (:word "FEXPM1")
  "(F: r1 – r2 )"
  "Raise e to the power R1 and subtract one, giving R2"
  (with-float-exceptions ()
    (stack-push float-stack (1- (exp (stack-pop float-stack))))))

(define-word ffield (:word "FFIELD:")
  "FFIELD: <name>" "( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first float aligned value greater than"
  "or equal to N1. N2 = OFFSET + 1 float. Create a definition for NAME with the execution semantics given below."
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((original-offset (stack-pop data-stack))
           (offset (if (zerop (mod original-offset +double-float-cell-size+))
                       original-offset
                       (+ original-offset (- +double-float-cell-size+ (mod original-offset +double-float-cell-size+)))))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (list offset))))
      (push word (fs-fields (stack-cell data-stack 0)))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset +double-float-cell-size+)))))

(define-word float-natural-log (:word "FLN")
  "(F: r1 – r2 )"
  "R2 is the natural logarithm of R1"
  (let ((r1 (stack-pop float-stack)))
    (if (plusp r1)
        (stack-push float-stack (log r1))
        (forth-exception :floating-out-of-range))))

(define-word float-plus-one-natural-log (:word "FLNP1")
  "(F: r1 – r2 )"
  "R2 is the natural logarithm of the quantity R1 plus one"
  ;;. An ambiguous condition exists if r1 is less than or equal to negative one
  (let ((r1 (1+ (stack-pop float-stack))))
    (if (plusp r1)
        (stack-push float-stack (log r1))
        (forth-exception :floating-out-of-range))))

(define-word float-log10 (:word "FLOG")
  "(F: r1 – r2 )"
  "R2 is the base-ten logarithm of R1"
  (let ((r1 (stack-pop float-stack)))
    (if (plusp r1)
        (stack-push float-stack (log r1 (native-float 10.0)))
        (forth-exception :floating-out-of-range))))

(define-word float-display-scientific (:word "FS.")
  "(F: r – )"
  "Display, with a trailing space, the top number on the floating-point stack in scientific notation"
  (unless (= base 10)
    (forth-exception :invalid-floating-base))
  (with-native-float-format ()
    (format t "~,V,,1E " (1- float-precision) (stack-pop float-stack))))

(define-word float-sin (:word "FSIN")
  "(F: r1 – r2 )"
  "R2 is the sine of the radian angle R1"
  (stack-push float-stack (sin (stack-pop float-stack))))

(define-word float-sin-cos (:word "FSINCOS")
  "(F: r1 – r2 r3 )"
  "R2 is the sine of the radian angle R1. R3 is the cosine of the radian angle R1"
  (let ((r1 (stack-pop float-stack)))
    (stack-push float-stack (sin r1))
    (stack-push float-stack (cos r1))))

(define-word float-sinh (:word "FSINH")
  "(F: r1 – r2 )"
  "R2 is the hyperbolic sine of R1"
  (with-float-exceptions ()
    (stack-push float-stack (sinh (stack-pop float-stack)))))

(define-word float-sqrt (:word "FSQRT")
  "(F: r1 – r2 )"
  "R2 is the square root of R1"
  (let ((r1 (stack-pop float-stack)))
    (if (minusp r1)
        (forth-exception :floating-out-of-range)
        (stack-push float-stack (sqrt r1)))))

(define-word float-tangent (:word "FTAN")
  "(F: r1 – r2 )"
  "R2 is the tangent of the radian angle R1"
  ;;. An ambiguous condition exists if cos(r1) is zero
  (with-float-exceptions ()
    (stack-push float-stack (tan (stack-pop float-stack)))))

(define-word float-tanh (:word "FTANH")
  "(F: r1 – r2 )"
  "R2 is the hyperbolic tangent of R1"
  (stack-push float-stack (tanh (stack-pop float-stack))))

(define-word float-truncate (:word "FTRUNC")
  "(F: r1 – r2 )"
  "Round R1 to an integral value using the \"round towards zero\" rule, giving R2"
  (stack-push float-stack (native-float (truncate (stack-pop float-stack)))))

(define-word float-value (:word "FVALUE")
  "FVALUE <name>" "(F: r – )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Create a definition for NAME with the execution semantics"
  "defined below, with an initial value equal to R. NAME is referred to as a \"f-value\""
  "NAME Execution: (F: – r )"
  "Place R on the floating point stack. The value of R is that given when NAME was created, until the phrase"
  "\"r TO name\" is executed, causing a new value of R to be assigned to NAME"
  "TO name Run-time: (F: r - )"
  "Assign the value R to NAME"
  (let ((name (word files #\Space))
        (value (stack-pop float-stack)))
    (when (null name)
      (forth-exception :zero-length-name))
    (align-memory memory +native-float-cell-size+)
    (let* ((address (allocate-memory memory +native-float-cell-size+))
           (word (make-word name #'push-value :parameters (list address :fvalue) :creating-word? t)))
      (setf (memory-native-float memory address) value)
      (add-and-register-word fs word address))))

(define-word float-proximate (:word "F~")
  "( – flag ) (F: r1 r2 r3 – )"
  "If R3 is positive, FLAG is true if the absolute value of (R1 minus R2) is less than R3"
  "If R3 is zero, FLAG is true if the implementation-dependent encoding of R1 and R2 are exactly identical"
  "(positive and negative zero are unequal if they have distinct encodings)"
  "If R3 is negative, FLAG is true if the absolute value of (R1 minus R2) is less than the absolute value of"
  "R3 times the sum of the absolute values of R1 and R2"
  (with-float-exceptions ()
    (let ((r3 (stack-pop float-stack))
          (r2 (stack-pop float-stack))
          (r1 (stack-pop float-stack)))
      (cond ((plusp r3)
             (if (< (abs (- r1 r2)) r3)
                 (stack-push data-stack +true+)
                 (stack-push data-stack +false+)))
            ((zerop r3)
             (if (= (decode-native-float r1) (decode-native-float r2))
                 (stack-push data-stack +true+)
                 (stack-push data-stack +false+)))
            ((minusp r3)
             (if (< (abs (- r1 r2)) (* (abs r3) (+ (abs r1) (abs r2))))
                 (stack-push data-stack +true+)
                 (stack-push data-stack +false+)))))))
               
(define-word float-precision (:word "PRECISION")
  "( – u )"
  "Return the number of significant digits currently used by F., FE., or FS. as U"
  (stack-push data-stack float-precision))

(define-word single-to-float (:word "S>F")
  "( n – ) (F: – r)"
  "R is the floating-point equivalent of the single cell value N"
  (let* ((single (cell-signed (stack-pop data-stack)))
         (float (native-float single)))
    (unless (= single float)
      (forth-exception :loss-of-precision))
    (stack-push float-stack float)))

(define-word set-float-precision (:word "SET-PRECISION")
  "( u – )"
  "Set the number of significant digits currently used by F., FE., or FS. to U"
  (let ((precision (cell-signed (stack-pop data-stack))))
    (when (minusp precision)
      (forth-exception :invalid-numeric-argument))
    (setf float-precision precision)))

(define-word short-float-store (:word "SF!")
  "(sf-addr – ) (F: r – )"
  "Store the floating-point number R as a 32-bit IEEE single-precision number at SF-ADDR. If the significand of the internal"
  "representation of R has more precision than the IEEE single-precision format, it will be rounded using the"
  "\"round to nearest\" rule"
  (with-float-exceptions ()
    (let ((address (stack-pop data-stack))
          (float (>single-float (stack-pop float-stack))))
      (setf (memory-single-float memory address) float))))

(define-word short-float-fetch (:word "SF@")
  "( sf-addr – ) (F: – r)"
  "Fetch the 32-bit IEEE single-precision number stored at SF-ADDR to the floating-point stack as R in the internal"
  "representation. If the IEEE single-precision significand has more precision than the internal representation,"
  "it will be rounded to the internal representation using the \"round to nearest\" rule"
  (with-float-exceptions ()
    (stack-push float-stack (native-float (memory-double-float memory (stack-pop data-stack))))))

(define-word sfloat-align (:word "SFALIGN")
  "If the data-space pointer is not single-float aligned, reserve enough data space to make it so"
  (align-memory +single-float-cell-size+))

(define-word sfloat-aligned (:word "SFALIGNED")
  "( addr – sf-addr )"
  "SF-ADDR is the first single-float-aligned address greater than or equal to ADDR"
  (let ((addr (stack-pop data-stack)))
    (stack-push data-stack
                (if (zerop (mod addr +single-float-cell-size+))
                    addr
                    (+ addr (- +single-float-cell-size+ (mod addr +single-float-cell-size+)))))))

(define-word sffield (:word "SFFIELD:")
  "SFFIELD: <name>" "( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first single-float aligned value greater than"
  "or equal to N1. N2 = OFFSET + 1 single-float. Create a definition for NAME with the execution semantics given below."
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((original-offset (stack-pop data-stack))
           (offset (if (zerop (mod original-offset +single-float-cell-size+))
                       original-offset
                       (+ original-offset (- +single-float-cell-size+ (mod original-offset +single-float-cell-size+)))))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (list offset))))
      (push word (fs-fields (stack-cell data-stack 0)))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset +single-float-cell-size+)))))

(define-word sfloat-plus (:word "SFLOAT+")
  "( sf-addr1 – sf-addr2 )"
  "Add the size in address units of a 32-bit IEEE single-precision number to SF-ADDR1, giving SF-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +single-float-cell-size+)))

(define-word sfloats (:word "SFLOATS")
  "( n1 – n2 )"
  "N2 is the size in address units of N1 32-bit IEEE single-precision numbers"
  (stack-push data-stack (* (stack-pop data-stack) +single-float-cell-size+)))
