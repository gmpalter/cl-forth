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

;;; Foreign Function Interface (FFI) words based on SwiftForth

(define-word ffi-library (:word "LIBRARY")
  "LIBRARY <filename>"
  "Use library FILENAME for subsequent imports via FUNCTION: and GLOBAL:"
  (let ((filename (word files #\Space)))
    (when (null filename)
      (forth-exception :zero-length-name))
    (load-foreign-library ffi filename)))

(define-word ffi-xlibrary (:word "XLIBRARY")
  "XLIBRARY <filename1> ... <filenameN>"
  "Select the approriate FILENAME based on the platform (i.e., \".so\" for Linux, \".dylib\" for macOS, \".dll\" for Windows)"
  "Execute LIBRARY for that FILENAME and treat the rest of the line as a comment"
  (let* ((target-suffix #+Linux "so"
                        #+Windows "dll"
                        #+Darwin "dylib")
         (libraries (loop for library = (word files #\Space)
                          while library
                          collect library))
         (library (find-if #'(lambda (library)
                               (labels ((match? (filename)
                                          (let ((type (pathname-type filename)))
                                            (or (equalp type target-suffix)
                                                ;; On Linux, library names may end with a version number rather than the
                                                ;; the "so" suffix (e.g., "libc.so.6", "libcurl.so.4.6.0"). As modern Lisps
                                                ;; will return the last component (e.g., "6", "0") as the pathname-type,
                                                ;; we need to recursively check the pathname-name to see if it has the
                                                ;; right suffix until the type is no longer numeric
                                                (when (multiple-value-bind (val end)
                                                          (parse-integer type :junk-allowed t)
                                                        (and (integerp val) (= end (length type))))
                                                  (match? (pathname-name filename)))))))
                                 (match? library)))
                           libraries)))
    (if library
        (load-foreign-library ffi library)
        (format t "~&Warning: No library for this platform specified in: XLIBRARY ~{~A ~}~%" libraries))))

;;; The form of a parameter list is
;;;
;;;  ( params -- return )
;;;
;;; The name given to each parameter and the return value is for documentation purposes only as all parameters
;;; are taken from the data stack or floating-point stack and the return value is placed on the data or floating-point stack.
;;;
;;; However, prefix character(s) determine the type of a parameter or the return value.
;;; If no prefix is present, the parameter or return value is a 64-bit signed integer
;;;
;;; The available prefixes and the CFFI equivalent data type are
;;;   *   :POINTER -- An address of data either in one of Forth's data spaces or the external data space
;;;   $   :INT32   -- 32-bit signed integer value taken/pushed from/to the data stack
;;;   $u  :UINT32  -- 32-bit unsigned integer value taken/pushed from/to the data stack
;;;   $$  :INT64   -- 64-bit signed integer value taken/pushed from/to the data stack
;;;   $$u :UINT64  -- 64-bit unsigned integer value taken/pushed from/to the data stack
;;;   %   :SINGLE  -- Single precision floating point value taken/pushed from/to the floating-point stack
;;;   %%  :DOUBLE  -- Double precision floating point value taken/pushed from/to the floating-point stack
;;;
;;; If the function does not return a value, omit the return value specification

(defun parse-parameters-and-return (fs what)
  (with-forth-system (fs)
    (let ((open (word files #\Space))
          (seen-dash-dash? nil)
          (parameters nil)
          (return-value :void))
      (unless (string-equal open "(")
        (forth-exception :invalid-foreign-parameter-list "\"(\" missing after ~A" what))
      (flet ((classify (item)
               (let* ((length (length item))
                      (ch0 (subseq item 0 1))
                      (ch0&1 (if (>= length 2) (subseq item 0 2) ""))
                      (ch0&1&2 (if (>= length 3) (subseq item 0 3) "")))
                 (cond ((string-equal ch0&1&2 "$$u") :uint64)
                       ((string-equal ch0&1 "$$") :int64)
                       ((string-equal ch0&1 "$u") :uint32)
                       ((string-equal ch0 "$") :int32)
                       ((string-equal ch0&1 "%%") :double)
                       ((string-equal ch0 "%") :single)
                       ((string-equal ch0 "*") :pointer)
                       (t :int64)))))
        (loop
          (let ((item (word files #\Space)))
            (cond ((null item)
                   (forth-exception :invalid-foreign-parameter-list "Incomplete parameters list for ~A" what))
                  ((string-equal item "--")
                   (if seen-dash-dash?
                       (forth-exception :invalid-foreign-parameter-list "Duplicate \"--\" after ~A" what)
                       (setq seen-dash-dash? t)))
                  ((string-equal item ")")
                   (if seen-dash-dash?
                       (return (values (reverse parameters) return-value))
                       (forth-exception :invalid-foreign-parameter-list "\")\" before \"--\" after ~A" what)))
                  (seen-dash-dash?
                   (if (eq return-value :void)
                       (setq return-value (classify item))
                       (forth-exception :invalid-foreign-parameter-list "Multiple return values after ~A" what)))
                  (t
                   (push (classify item) parameters)))))))))

(defun ffi-define-function (fs &key forth-name optional?)
  (with-forth-system (fs)
    (when (null (ffi-current-library ffi))
      (forth-exception :no-foreign-library))
    (let ((name (word files #\Space)))
      (when (null name)
        (forth-exception :zero-length-name))
      (unless optional?
        (when (null (cffi:foreign-symbol-pointer name :library (library-ffi-library (ffi-current-library ffi))))
          (forth-exception :undefined-foreign-function "Foreign function ~A~@[ (AS ~A)~] is not defined~@[ in ~A~]"
                           name forth-name  #+LispWorks (library-name (ffi-current-library ffi)) #-LispWorks nil)))
      (multiple-value-bind (parameters return-value)
          (parse-parameters-and-return fs (format nil "~:[~;[OPTIONAL] ~]~@[AS ~A ~]FUNCTION: ~A" optional? forth-name name))
        (multiple-value-bind (code forms)
            (build-ffi-call ffi name (ffi-current-library ffi) parameters return-value optional?)
          (let ((word (make-word (or forth-name name) code :parameters (make-parameters forth-name))))
            (when forms
              (setf (word-inline-forms word) forms
                    (word-inlineable? word) t))
            (add-and-register-word fs word)))))))

(define-word ffi-function (:word "FUNCTION:")
  "FUNCTION: <name> ( params -- return )"
  "Define a Forth word to call the foreign function NAME which expects the specified parameters and returns zero or one values"
  "The name of the Forth word is NAME unless FUNCTION: is preceeded by AS <forth-name>"
  (ffi-define-function fs))

(defun ffi-define-global (fs &key forth-name optional?)
  (with-forth-system (fs)
    (when (null (ffi-current-library ffi))
      (forth-exception :no-foreign-library))
    (let ((name (word files #\Space)))
      (when (null name)
        (forth-exception :zero-length-name))
      (unless optional?
        (when (null (cffi:foreign-symbol-pointer name :library (library-ffi-library (ffi-current-library ffi))))
          (forth-exception :undefined-foreign-global "Foreign global ~A~@[ (AS ~A)~] is not defined~@[ ~A~]"
                           name forth-name #+LispWorks (library-name (ffi-current-library ffi)) #-LispWorks nil)))
      (let ((word (make-word (or forth-name name) #'push-parameter-as-global-pointer
                             :parameters (make-parameters name forth-name (ffi-current-library ffi)))))
        (add-and-register-word fs word)))))
  
(define-word ffi-global (:word "GLOBAL:")
  "GLOBAL: <name>"
  "Define a Forth word to return the address of the external global variable NAME"
  "The name of the Forth word is NAME unless GLOBAL: is preceeded by AS <forth-name>"
  (ffi-define-global fs))

(defun ffi-do-rename (fs &key optional?)
  (with-forth-system (fs)
    (let* ((forth-name (word files #\Space))
           (definition (word files #\Space)))
      (when (null forth-name)
        (forth-exception :zero-length-name))
      (cond ((string-equal definition "FUNCTION:")
             (ffi-define-function fs :forth-name forth-name :optional? optional?))
            ((string-equal definition "GLOBAL:")
             (ffi-define-global fs :forth-name forth-name :optional? optional?))
            ((string-equal definition "[OPTIONAL]")
             (when optional?
               (forth-exception :duplicate-optional-clauses))
             (ffi-do-optional fs :forth-name forth-name))
            ((string-equal definition "AS")
             (forth-exception :duplicate-as-clauses))
            ((null definition)
             (forth-exception :missing-foreign-definition "AS must be followed by [OPTIONAL], FUNCTION:, or GLOBAL:"))
            (t
             (forth-exception :missing-foreign-definition "AS must be followed by [OPTIONAL], FUNCTION:, or GLOBAL:, not ~A"
                              definition))))))

(define-word ffi-rename (:word "AS")
  "AS <name> <definition>"
  "Provide a different Forth name for a FUNCTION: or GLOBAL: definition"
  (ffi-do-rename fs))

(defun ffi-do-optional (fs &key forth-name)
  (with-forth-system (fs)
    (let* ((definition (word files #\Space)))
      (cond ((string-equal definition "FUNCTION:")
             (ffi-define-function fs :forth-name forth-name :optional? t))
            ((string-equal definition "GLOBAL:")
             (ffi-define-global fs :forth-name forth-name :optional? t))
            ((string-equal definition "LIBRARY")
             (let ((filename (word files #\Space)))
               (when (null filename)
                 (forth-exception :zero-length-name))
               (load-foreign-library ffi filename :optional? t)))
            ((string-equal definition "AS")
             (when forth-name
               (forth-exception :duplicate-as-clauses))
             (ffi-do-rename fs :optional? t))
            ((string-equal definition "[OPTIONAL]")
             (forth-exception :duplicate-optional-clauses))
            ((null definition)
             (forth-exception :missing-foreign-definition "[OPTIONAL] must be followed by AS, LIBRARY, FUNCTION:, or GLOBAL:"))
            (t
             (forth-exception :missing-foreign-definition
                              "[OPTIONAL] must be followed by AS, LIBRARY, FUNCTION:, or GLOBAL:, not ~A"
                              definition))))))

(define-word ffi-optional (:word "[OPTIONAL]")
  "[OPTIONAL] <definition>"
  "Mark the next definition, a LIBRARY, FUNCTION: or GLOBAL:, as optional"
  (ffi-do-optional fs))

(define-word show-libraries (:word ".LIBS")
  "Displays a list of all available libraries opened by LIBRARY or XLIBRARY"
  (cond ((zerop (length (ffi-libraries ffi)))
         (write-line "No foreign libraries"))
        (t
         (write-line "Foreign Libraries:")
         (loop for library across (ffi-libraries ffi)
               if (eq (library-ffi-library library) :default)
                 do (write-line " [Default]")
               else
                 do (format t "~& ~A~@[ (not loaded)~]~%" (library-name library) (library-not-loaded? library))))))

(define-word show-imports (:word ".IMPORTS")
  "Displays a list of all currently available functions imported by FUNCTION:"
  (let ((imports nil))
    (maphash #'(lambda (name ffi-call)
                 (declare (ignore name))
                 (push ffi-call imports))
             (ffi-ffi-calls ffi))
    (cond ((zerop (length imports))
           (write-line "No imported functions"))
          (t
           (setf imports (sort imports #'(lambda (f1 f2)
                                           (let ((p1 (cffi:foreign-symbol-pointer (ffi-call-name f1)))
                                                 (p2 (cffi:foreign-symbol-pointer (ffi-call-name f2))))
                                             (cond ((and p1 p2)
                                                    (< (pointer-address p1) (pointer-address p2)))
                                                   ((and (null p1) (null p2))
                                                    (string-lessp (ffi-call-name f1) (ffi-call-name f2)))
                                                   ((null p1) nil)
                                                   (t t))))))
           (write-line "Imported functions:")
           (dolist (import imports)
             (format t "~&  $~16,'0X ~@[~A ~]~A~%"
                     (let ((pointer (cffi:foreign-symbol-pointer (ffi-call-name import))))
                       (if pointer (pointer-address pointer) "     <Undefined>"))
                     #+LispWorks (library-name (ffi-call-library import)) #-LispWorks nil
                     (ffi-call-name import)))))))

(define-word ffi-callback (:word "CALLBACK:")
  "CALLBACK: <name> ( params -- return )" "( xt -- )"
  "Creates an FFI callback which calls the execution token XT. The parameters specified by PARAMS are pushed"
  "on the data stack and/or floating-point stack as appropriate. The callback returns zero or one value as"
  "specified by RETURN."
  "Defines the word NAME which pushes the address of the callback onto the data stack"
  (let ((xt (stack-pop data-stack))
        (name (word files #\Space)))
    (verify-execution-token execution-tokens xt)
    (when (null name)
      (forth-exception :zero-length-name))
    (multiple-value-bind (parameters return-value)
        (parse-parameters-and-return fs (format nil "CALLBACK: ~A" name))
      (let* ((callback (build-ffi-callback ffi fs name xt parameters return-value))
             (word (make-word name #'push-parameter-as-callback-ptr :parameters (make-parameters callback))))
        (add-and-register-word fs word)))))


;;; BEGIN-NAMED-STRUCTURE and additional field defining words

(define-word begin-named-structure (:word "BEGIN-NAMED-STRUCTURE")
  "BEGIN-NAMED-STRUCTURE <name>" "( -- struct-sys 0 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. Create a definition for NAME with the execution semantics"
  "defined below. Return a STRUCT-SYS (zero or more implementation dependent items) that will be used by END-STRUCTURE and an"
  "initial offset of 0."
  "Fields added to this structure will have the structure's name prepended to the field name (e.g., \"STR.FIELD\")"
  "NAME Execution: ( -- +n )"
  "+N is the size in memory expressed in address units of the data structure"
  (let ((name (word files #\Space))
        (struct (make-forth-structure :named? t)))
    (when (null name)
      (forth-exception :zero-length-name))
    (let ((word (make-word name #'push-structure-size-from-parameter :smudge? t :parameters (make-parameters struct))))
      (setf (fs-word struct) word)
      (add-and-register-word fs word)
      (stack-push data-stack struct)
      (stack-push data-stack 0))))

(define-word define-word-field (:word "WFIELD:")
  "WFIELD: <name> -- ( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first 2-byte (word) aligned value"
  "greater than or equal to N1. N2 = OFFSET + 1 word. Create a definition for STR.NAME with the execution semantics below"
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (add-structure-field fs name 2)))

(define-word define-long-field (:word "LFIELD:")
  "LFIELD: <name> -- ( n1 – n2 )"
  "Skip leading space delimiters. Parse NAME delimited by a space. OFFSET is the first 4-byte (longword) aligned value"
  "greater than or equal to N1. N2 = OFFSET + 1 longword. Create a definition for NAME with the execution semantics below"
  "NAME Execution: ( addr1 – addr2 )"
  "Add the offset calculated during the compile-time action to ADDR1 giving the address ADDR2"
  (let ((name (word files #\Space)))
    (when (null name)
      (forth-exception :zero-length-name))
    (add-structure-field fs name 4)))


;;; Memory access for smaller than cell sized data
;;; ---*** NOTE: Some of these words have been proposed as additions to Standard Forth but with slightly different
;;;              semantics. We will reconcile those differences if and when they are accepted into the standard.

(define-word fetch-word (:word "W@")
  "( addr -- x )"
  "Fetch 2 bytes (1 word) stored at ADDR. When the cell size is greater than 2 bytes, propogate the high-order"
  "bit of the high-order byte into the unused high-order bits of the cell"
  (stack-push data-stack (memory-double-byte memory (stack-pop data-stack))))

(define-word fetch-unsigned-word (:word "UW@")
  "( addr -- x )"
  "Fetch 2 bytes (1 word) stored at ADDR. When the cell size is greater than 2 bytes, the unused high-order bits"
  "are all zeroes"
  (stack-push data-stack (double-byte-unsigned (memory-double-byte memory (stack-pop data-stack)))))

(define-word store-word (:word "W!")
  "( x addr -- )"
  "Store the low-order 2 bytes (1 word) of X at ADDR"
  (setf (memory-double-byte memory (stack-pop data-stack)) (stack-pop data-stack)))

(define-word allocate-word (:word "W,")
  "( x -- )"
  "Reserve space for 2 bytes (1 word) in data space and store the low-order 2 bytes of X in the space"
  (flush-optimizer-stack :contains (data-space-high-water-mark memory))
  (setf (memory-double-byte memory (allocate-memory memory 2)) (stack-pop data-stack)))

(define-word fetch-long (:word "L@")
  "( addr -- x )"
  "Fetch 4 bytes (1 long) stored at ADDR. When the cell size is greater than 4 bytes, propogate the high-order"
  "bit of the high-order byte into the unused high-order bits of the cell"
  (stack-push data-stack (memory-quad-byte memory (stack-pop data-stack))))

(define-word fetch-unsigned-long (:word "UL@")
  "( addr -- x )"
  "Fetch 4 bytes (1 long) stored at ADDR. When the cell size is greater than 4 bytes, the unused high-order bits"
  "are all zeroes"
  (stack-push data-stack (quad-byte-unsigned (memory-quad-byte memory (stack-pop data-stack)))))

(define-word store-long (:word "L!")
  "( x addr -- )"
  "Store the low-order 4 bytes (1 long) of X at ADDR"
  (setf (memory-quad-byte memory (stack-pop data-stack)) (stack-pop data-stack)))

(define-word allocate-long (:word "L,")
  "( x -- )"
  "Reserve space for 4 bytes (1 long) in data space and store the low-order 4 bytes of X in the space"
  (flush-optimizer-stack :contains (data-space-high-water-mark memory))
  (setf (memory-quad-byte memory (allocate-memory memory 4)) (stack-pop data-stack)))

(define-word fetch-pointer (:word "P@")
  "( addr -- x )"
  "Fetch X from ADDR. X is interpreted as a pointer to, possibly, foreign memory"
  (stack-push data-stack (native-address memory (address-pointer (memory-cell memory (stack-pop data-stack))))))

(define-word store-pointer (:word "P!")
  "( x addr -- )"
  "Store X at ADDR. X is interpreted as a pointer to, possibly, foreign memory"
  (setf (memory-cell memory (stack-pop data-stack)) (pointer-address (foreign-pointer memory (stack-pop data-stack)))))
