(in-package #:forth)

;;; CL-Forth uses double precision floating point as its internal representation of float values

;;; Floating-Point words as defined in Section 12 of the Forth 2012 specification

(defmacro with-float-exceptions (() &body body)
  `(handler-case
       (progn ,@body)
     (floating-point-overflow ()
       (forth-exception :floating-out-of-range))
     (floating-point-underflow ()
       (forth-exception :float-underflow))
     (floating-point-inexact ()
       (forth-exception :loss-of-precision))
     (floating-point-invalid-operation ()
       (forth-exception :float-unknown-fault))))

;;;---*** >FLOAT

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

;;;---*** FCONSTANT

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

;;;---*** FLITERAL

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

;;;---*** FVARIABLE
;;;---*** REPRESENT


;;; Floating-Point extension  words as defined in Section 12 of the Forth 2012 specification

(define-word dfloat-store (:word "DF!")
  "( df-addr – ) (F: r – )"
  "Store the floating-point number R as a 64-bit IEEE double-precision number at DF-ADDR"
  (with-float-exceptions ()
    (let ((float (double-float (stack-pop float-stack)))
          (address (stack-pop data-stack)))
      (setf (memory-double-float memory address) float))))

(define-word dfloat-fetch (:word "DF@")
  "( df-addr – ) (F: – r)"
  "Fetch the 64-bit IEEE double-precision number stored at DF-ADDR to the floating-point stack as R in"
  "the internal representation"
  (with-float-exceptions
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
      (stack-push data-stack (expt r1 r2)))))

;;;---*** F.

(define-word float-to-single (:word "F>S")
  "( – n ) (F: r – )"
  "N is the single-cell signed-integer equivalent of the integer portion of R, rounded towards zero."
  "The fractional portion of R is discarded."
  (let ((single (truncate (stack-pop float-stack))))
    (if (<= +most-negative-single-cell+ double +most-positive-single-cell+)
        (stack-push data-stack single)
        (forth-exception :floating-out-of-range))))

;;;---*** FABS
;;;---*** FACOS
;;;---*** FACOSH
;;;---*** FALOG
;;;---*** FASIN
;;;---*** FASINH
;;;---*** FATAN
;;;---*** FATAN2
;;;---*** FATANH
;;;---*** FCOS
;;;---*** FCOSH
;;;---*** FE.
;;;---*** FEXP
;;;---*** FEXPM1

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

;;;---*** FLN
;;;---*** FLNP1
;;;---*** FLOG
;;;---*** FS.
;;;---*** FSIN
;;;---*** FSINCOS
;;;---*** FSINH
;;;---*** FSQRT
;;;---*** FTAN
;;;---*** FTANH
;;;---*** FTRUNC
;;;---*** FVALUE
;;;---*** F~
;;;---*** PRECISION

(define-word single-to-float (:word "S>F")
  "( n – ) (F: – r)"
  "R is the floating-point equivalent of the single cell value N"
  (let* ((single (cell-signed (stack-pop data-stack)))
         (float (native-float single)))
    (unless (= single float)
      (forth-exception :loss-of-precision))
    (stack-push float-stack float)))

;;;---*** SET-PRECISION

(define-word short-float-store (:word "SF!")
  "(sf-addr – ) (F: r – )"
  "Store the floating-point number R as a 32-bit IEEE single-precision number at SF-ADDR. If the significand of the internal"
  "representation of R has more precision than the IEEE single-precision format, it will be rounded using the"
  "\"round to nearest\" rule"
  (with-float-exceptions ()
    (let ((address (stack-pop data-stack))
          (float (single-float (stack-pop float-stack))))
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
