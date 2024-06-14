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
         (library (find target-suffix libraries :key #'pathname-type :test #'equalp)))
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
          (forth-exception :undefined-foreign-function "~A is not defined in ~A"
                           name (library-name (ffi-current-library ffi)))))
      (multiple-value-bind (parameters return-value)
          (parse-parameters-and-return fs "FUNCTION:")
        (let* ((code (build-ffi-call ffi name (ffi-current-library ffi) parameters return-value))
               (word (make-word (or forth-name name) code)))
          (add-and-register-word fs word))))))

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
          (forth-exception :undefined-foreign-global "~A is not defined in ~A" name (library-name (ffi-current-library ffi)))))
      (let ((word (make-word (or forth-name name) #'push-parameter-as-global-pointer
                             :parameters (list name (ffi-current-library ffi)))))
        (add-and-register-word fs word)))))
  
(define-word ffi-global (:word "GLOBAL:")
  "GLOBAL: <name>"
  "Define a Forth word to return the address of the external global variable NAME"
  "The name of the Forth word is NAME unless GLOBAL: is preceeded by AS <forth-name>"
  (ffi-define-global fs))

(define-word ffi-rename (:word "AS")
  "AS <name> <definition>"
  "Provide a different Forth name for a FUNCTION: or GLOBAL: definition"
  (let* ((forth-name (word files #\Space))
         (definition (word files #\Space)))
    (when (null forth-name)
      (forth-exception :zero-length-name))
    (cond ((string-equal definition "FUNCTION:")
           (ffi-define-function fs :forth-name forth-name))
          ((string-equal definition "GLOBAL:")
           (ffi-define-global fs :forth-name forth-name))
          ((null definition)
           (forth-exception :missing-foreign-definition))
          (t
           (forth-exception :missing-foreign-definition "AS must be followed by FUNCTION: or GLOBAL:, not ~A" definition)))))

#+TODO
(define-word ffi-optinal (:word "[OPTIONAL]")
  "[OPTIONAL] <definition>"
  ""
  )

(define-word show-libraries (:word ".LIBS")
  "Displays a list of all available libraries opened by LIBRARY or XLIBRARY"
  (cond ((zerop (length (ffi-libraries ffi)))
         (write-line "No foreign libraries"))
        (t
         (write-line "Foreign Libraries:")
         (loop for library across (ffi-libraries ffi)
               do (format t "~& ~A~%" (library-name library))))))

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
                                           (< (pointer-address (cffi:foreign-symbol-pointer (ffi-call-name f1)))
                                              (pointer-address (cffi:foreign-symbol-pointer (ffi-call-name f2)))))))
           (write-line "Imported functions:")
           (dolist (import imports)
             (format t "~&  ~16,'0X ~A ~A~%"
                     (pointer-address (cffi:foreign-symbol-pointer (ffi-call-name import)))
                     (library-name (ffi-call-library import))
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
        (parse-parameters-and-return fs "FUNCTION:")
      (let* ((callback (build-ffi-callback ffi fs name xt parameters return-value))
             (word (make-word name #'push-parameter-as-callback-ptr :parameters (list callback))))
        (add-and-register-word fs word)))))
