(in-package #:forth)

;;; Facility words as defined in Section 10 of the Forth 2012 specification

;;;---*** AT-XY
;;;---*** KEY?
;;;---*** PAGE


;;; Facility extension words as defined in Section 10 of the Forth 2012 specification

(define-word +field (:word "+FIELD")
  "+FIELD <name>" "( n1 n2 - n3 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Create a definition for NAME with the execution semantics"
  "defined below. Return N3 = N1 + N2 where N1 is the offset in the data structure before +FIELD executes, and N2 is the size"
  "of the data to be added to the data structure. N1 and N2 are in address units."
  "NAME Execution: ( addr1 – addr2 )"
  "Add N1 to ADDR1 giving ADDR2"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((size (stack-pop data-stack))
           (offset (stack-pop data-stack))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (list offset))))
      (push word (fs-fields (stack-cell data-stack 0)))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset size)))))

(define-word begin-structure (:word "BEGIN-STRUCTURE")
  "BEGIN-STRUCTURE <name>" "( - struct-sys 0 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Create a definition for NAME with the execution semantics"
  "defined below. Return a STRUCT-SYS (zero or more implementation dependent items) that will be used by END-STRUCTURE and an"
  "initial offset of 0"
  "NAME Execution: ( - +n )"
  "+N is the size in memory expressed in address units of the data structure"
  (let ((name (word files #\Space))
        (struct (make-forth-structure)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'push-structure-size-from-parameter :smudge? t :parameters (list struct))))
      (setf (fs-word struct) word)
      (add-and-register-word fs word)
      (stack-push data-stack struct)
      (stack-push data-stack 0))))

(define-word cfield (:word "CFIELD:")
  "CFIELD: <name>" "( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first character aligned value greater than"
  "or equal to N1. N2 = OFFSET + 1 character. Create a definition for NAME with the execution semantics given below."
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((original-offset (stack-pop data-stack))
           (offset (if (zerop (mod original-offset +char-size+))
                       original-offset
                       (+ original-offset (- +char-size+ (mod original-offset +char-size+)))))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (list offset))))
      (push word (fs-fields (stack-cell data-stack 0)))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset +char-size+)))))

;;;---*** EKEY
;;;---*** EKEY>CHAR
;;;---*** EKEY>FKEY
;;;---*** EKEY?
;;;---*** EMIT?

(define-word end-structure (:word "END-STRUCTURE")
  "( struct-sys +n – )"
  "Terminate definition of a structure started by BEGIN-STRUCTURE"
  (let* ((size (stack-pop data-stack))
         (struct (stack-pop data-stack))
         (struct-word (fs-word struct)))
    (setf (fs-size struct) size)
    (setf (word-inline-forms struct-word) `((stack-push data-stack ,size))
          (word-inlineable? struct-word) t
          (word-smudge? (fs-word struct)) nil)
    (map nil #'(lambda (field)
                 (setf (word-inline-forms field) `((stack-push data-stack (+ (stack-pop data-stack)
                                                                             ,(first (word-parameters field)))))
                       (word-inlineable? field) t
                       (word-smudge? field) nil))
         (fs-fields struct))))

(define-word field (:word "FIELD:")
  "FIELD: <name>" "( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first cell aligned value greater than"
  "or equal to N1. N2 = OFFSET + 1 cell. Create a definition for NAME with the execution semantics given below."
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2."
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let* ((original-offset (stack-pop data-stack))
           (offset (if (zerop (mod original-offset +cell-size+))
                       original-offset
                       (+ original-offset (- +cell-size+ (mod original-offset +cell-size+)))))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (list offset))))
      (push word (fs-fields (stack-cell data-stack 0)))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset +cell-size+)))))

;;;---*** K-ALT-MASK, K-CTRL-MASK, K-DELETE, K-DOWN, K-END, K-F1, K-F10, K-F11, K-F12, K-F2, K-F3, K-F4, K-F5
;;;---*** K-F6, K-F7, K-K8, K-F9, K-HOME, K-INSERT, K-LEFT, K-NEXT, K-PRIOR, K-RIGHT, K-SHIFT-MASK, K-UP

(define-word sleep (:word "MS")
  "( u – )"
  "Wait at least U milliseconds"
  (let ((milliseconds (cell-signed (stack-pop data-stack))))
    (when (minusp milliseconds)
      (forth-exception :invalid-numeric-argument "Sleep interval can't be negative in MS"))
    (sleep (/ (float milliseconds 1.0d0) 1000.0d0))))

(define-word date-time (:word "TIME&DATE")
  "( – +n1 +n2 +n3 +n4 +n5 +n6 )"
  "Return the current time and date. +N1 is the second {0. . . 59}, +N2 is the minute {0. . . 59}, +N3 is the hour {0...23},"
  "+N4 is the day {1...31}, +N5 is the month {1...12} and +N6 is the year (e.g., 1991)"
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (stack-push data-stack second)
    (stack-push data-stack minute)
    (stack-push data-stack hour)
    (stack-push data-stack date)
    (stack-push data-stack month)
    (stack-push data-stack year)))

