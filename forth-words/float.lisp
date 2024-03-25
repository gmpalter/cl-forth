(in-package #:forth)

;;; CL-Forth uses double precision floating point as its internal representation of float values

;;; Floating-Point words as defined in Section 12 of the Forth 2012 specification

;;;---*** TODO: Add a float exception handling macro
;;;  (:invalid-floating-base -40 "Invalid BASE for floating point conversion")
;;;  (:loss-of-precision -41 "Loss of precision")
;;;  (:floating-divide-by-zero -42 "Floating-point divide by zero")
;;;  (:floating-out-of-range -43 "Floating-point result out of range")
;;;  (:float-stack-overflow -44 "Floating-point stack overflow")
;;;  (:float-stack-underflow -45 "Floating-point stack underflow")
;;;  (:float-invalid-argument -46 "Floating-point invalid argument")
;;;  (:float-underflow -54 "Floating-point underflow")
;;;  (:float-unknown-fault -55 "Floating-point unidentified fault")

;;;---*** >FLOAT

(define-word double-to-float (:word "D>F")
  "( d – ) (F: – r)"
  "R is the floating-point equivalent of D"
  (stack-push float-stack (native-float (stack-pop-double data-stack))))

(define-word float-store (:word "F!")
  "( f-addr – ) (F: r – )"
  "Store R at F-ADDR"
  (let ((float (stack-pop float-stack))
        (address (stack-pop data-stack)))
    (setf (memory-native-float memory address) float)))

;;;---*** F*
;;;---*** F+
;;;---*** F-
;;;---*** F/
;;;---*** F0<
;;;---*** F0=
;;;---*** F<
;;;---*** F>D

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
;;;---*** FDEPTH
;;;---*** FDROP
;;;---*** FDUP
;;;---*** FLITERAL

(define-word float-plus (:word "FLOAT+")
  "( f-addr1 – f-addr2 )"
  "Add the size in address units of a floating-point number to F-ADDR1, giving f-ADDR2"
  (stack-push data-stack (+ (stack-pop data-stack) +native-float-cell-size+)))

(define-word floats (:word "FLOATS")
  "( n1 – n2 )"
  "N2 is the size in address units of N1 floating-point numbers"
  (stack-push data-stack (* (stack-pop data-stack) +native-float-cell-size+)))

;;;---*** FLOOR
;;;---*** FMAX
;;;---*** FMIN
;;;---*** FNEGATE
;;;---*** FOVER
;;;---*** FROT
;;;---*** FROUND
;;;---*** FSWAP
;;;---*** FVARIABLE
;;;---*** REPRESENT


;;; Floating-Point extension  words as defined in Section 12 of the Forth 2012 specification

(define-word dfloat-store (:word "DF!")
  "( df-addr – ) (F: r – )"
  "Store the floating-point number R as a 64-bit IEEE double-precision number at DF-ADDR"
  (let ((float (double-float (stack-pop float-stack)))
        (address (stack-pop data-stack)))
    (setf (memory-double-float memory address) float)))

(define-word dfloat-fetch (:word "DF@")
  "( df-addr – ) (F: – r)"
  "Fetch the 64-bit IEEE double-precision number stored at DF-ADDR to the floating-point stack as R in"
  "the internal representation"
  (stack-push float-stack (native-float (memory-double-float memory (stack-pop data-stack)))))

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

;;;---*** F**
;;;---*** F.
;;;---*** F>S
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
;;;---*** S>F
;;;---*** SET-PRECISION
;;;---*** SF!
;;;---*** SF@

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
