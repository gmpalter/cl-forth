(in-package #:forth)

(defvar *forth-exceptions-map* (make-hash-table))

(defmacro define-forth-exceptions (&body body)
  `(progn
    (clrhash *forth-exceptions-map*)
    (dolist (exception ',body)
      (destructuring-bind (key code default-phrase)
          exception
        (setf (gethash key *forth-exceptions-map*) (list code default-phrase))))))

(define-forth-exceptions
  (:abort -1 "ABORT")
  (:abort\" -2 "ABORT\"")
  (:stack-overflow -3 "Stack overflow")
  (:stack-underflow -4 "Stack underflow")
  (:return-stack-overflow -5 "Return stack overflow")
  (:return-stack-underflow -6 "Return stack underflow")
  (:do-loops-nesting -7 "DO loops nested too deeply during execution")
  (:dictionary-overflow -8 "Dictionary overflow")
  (:invalid-memory -9 "Invalid memory address")
  (:divide-by-zero -10 "Division by zero")
  (:out-of-range -11 "Result out of range")
  (:type-mismatch -12 "Argument type mismatch")
  (:undefined-word -13 "Undefined word")
  (:compile-only-word -14 "Interpreting a compile-only word")
  (:invalid-forget -15 "Invalid FORGET")
  (:zero-length-name -16 "Attempt to use zero-length string as a name")
  (:pictured-output-overflow -17 "Pictured numeric output string overflow")
  (:parse-string-overflow -18 "Parsed string overflow")
  (:name-too-long -19 "Definition name too long")
  (:write-to-read-only-memory -20 "Write to a read-only location")
  (:unsuppored-operation -21 "Unsupported operation")
  (:control-mismatch -22 "Control structure mismatch")
  (:aligment-exception -23 "Address alignment exception")
  (:invalid-numeric-argument -24 "Invalid numeric argument")
  (:return-stack-imbalance -25 "Return stack imbalance")
  (:no-loop-parameters -26 "Loop parameters unavailable")
  (:invalid-recursion -27 "Invalid recursion")
  (:user-interrupt -28 "User interrupt")
  (:recursive-compile -29 "Compiler nesting")
  (:obsolete-feature -30 "Obsolescent feature")
  (:invalid->body -31 ">BODY used on non-CREATEd definition")
  (:invalid-name-argument -32 "Invalid name argument")
  (:block-read-exception -33 "Block read exception")
  (:block-write-exception -34 "Block write exception")
  (:invalid-block-number -35 "Invalid block number")
  (:invalid-file-position -36 "Invalid file position")
  (:file-i/o-exception -37 "File I/O exception")
  (:file-not-found -38 "Non-existent file")
  (:unexpected-eof -39 "Unexpected end of file")
  (:invalid-floating-base -40 "Invalid BASE for floating point conversion")
  (:loss-of-precision -41 "Loss of precision")
  (:floating-divide-by-zero -42 "Floating-point divide by zero")
  (:floating-out-of-range -43 "Floating-point result out of range")
  (:float-stack-overflow -44 "Floating-point stack overflow")
  (:float-stack-underflow -45 "Floating-point stack underflow")
  (:float-invalid-argument -46 "Floating-point invalid argument")
  (:compilation-word-list-deleted -47 "Compilation word list deleted")
  (:invalid-postpone -48 "Invalid POSTPONE")
  (:search-order-overflow -49 "Search-order overflow")
  (:search-order-underflow -50 "Search-order underflow")
  (:compilation-word-list-changed -51 "Compilation word list changed")
  (:control-flow-stack-overflow -52 "Control-flow stack overflow")
  (:exception-stack-overflow -53 "Exception stack overflow")
  (:float-underflow -54 "Floating-point underflow")
  (:float-unknown-fault -55 "Floating-point unidentified fault")
  (:quit -56 "QUIT")
  (:send/receive-char-exception -57 "Exception in sending or receiving a character")
  (:if/then/else-exception -58 "[IF], [ELSE], or [THEN] exception")
  (:allocate-exception -59 "ALLOCATE exception")
  (:free-exception -60 "FREE exception")
  (:resize-exception -61 "RESIZE exception")
  (:close-file-exception -62 "CLOSE-FILE exception")
  (:create-file-exception -63 "CREATE-FILE exception")
  (:delete-file-exception -64 "DELETE-FILE exception")
  (:file-position-exception -65 "FILE-POSITION exception")
  (:file-size-exception -66 "FILE-SIZE exception")
  (:file-status-exception -67 "FILE-STATUS exception")
  (:flush-file-exception -68 "FLUSH-FILE exception")
  (:open-file-exception -69 "OPEN-FILE exception")
  (:read-file-exception -70 "READ-FILE exception")
  (:read-line-exception -71 "READ-LINE exception")
  (:rename-file-exception -72 "RENAME-FILE exception")
  (:reposition-file-exception -73 "REPOSITION-FILE exception")
  (:resize-file-exception -74 "RESIZE-FILE exception")
  (:write-file-exception -75 "WRITE-FILE exception")
  (:write-line-exception -76 "WRITE-LINE exception")
  (:malformed-xchar -77 "Malformed extended character")
  (:substitute-exception -78 "SUBSTITUTE exception")
  (:replaces-exception -79 "REPLACES exception")
  ;;
  ;; CL-Forth specific exceptions
  (:unknown-slot -256 "Unknown slot")
  (:control-flow-stack-underflow -257 "Control-flow stack empty")
  (:unknown-word-list -258 "Unknown word list")
  (:duplicate-word-list -259 "A word list by that name already exists")
  (:source-stack-overflow -260 "Input source stack overflow")
  (:source-stack-underflow -261 "Input source stack underflow")
  (:not-compiling -262 "Not compiling a definition")
  (:parse-integer-failure -263 "Conversion to an integer failed")
  (:optional-not-in-file -264 "OPTIONAL can only be used when including a file")
  (:no-execution-token -265 "No execution token available")
  (:recursive-pictured-output -266 "Pictured output already in progress")
  (:no-pictured-output -267 "Pictured output not in progress")
  (:definitions-stack-overflow -268 "Too many DOES> words in definition")
  (:definitions-stack-underflow -269 "Internal error: definitions stack underflow")
  (:save-restore-input-mismatch -270 "Saved input source doesn't match current source")
  (:not-defer -271 "Not a DEFER definition")
  (:defer-not-set -272 "Execution token not set in DEFER definition")
  (:not-a-name-token -273 "Not a name token")
  (:exception-stack-underflow -274 "Exception stack underflow")
  (:loop-stack-underflow -275 "DO loops stack underflow")
  )

(define-condition forth-exception (error)
  ((key :initarg :key :reader forth-exception-key)
   (code :initarg :code :reader forth-exception-code)                                                 
   (phrase :initarg :phrase :reader forth-exception-phrase))
  (:report (lambda (fe stream)
             (format stream "Forth exception ~D: ~A" (forth-exception-code fe) (forth-exception-phrase fe)))))

(defun forth-exception (key &optional phrase &rest phrase-arguments)
  (let ((entry (gethash key *forth-exceptions-map*))
        (phrase (and phrase (apply #'format nil phrase phrase-arguments))))
    (if entry
        (destructuring-bind (code default-phrase) entry
          (error 'forth-exception :key key :code code :phrase (or phrase default-phrase)))
        (error 'forth-exception :key :bad-exception-key :code -999 :phrase (format nil "Unrecognized exception key ~S" key)))))

(defun forth-exception-key-to-code (key)
  (let ((entry (gethash key *forth-exceptions-map*)))
    (and entry
        (destructuring-bind (code default-phrase) entry
          (declare (ignore default-phrase))
          code))))

(defun forth-exception-key-to-phrase (key)
  (let ((entry (gethash key *forth-exceptions-map*)))
    (and entry
        (destructuring-bind (code default-phrase) entry
          (declare (ignore code))
          default-phrase))))

(defun forth-exception-by-code (code)
  (let ((key (block find-key
               (maphash #'(lambda (key entry) (when (= (car entry) code) (return-from find-key key))) *forth-exceptions-map*)
               nil)))
    (cond (key
           (forth-exception key))
          ((plusp code)
           (error 'forth-exception :key :user-defined :code code :phrase (format nil "User defined exception code ~D" code)))
          (t
           (error 'forth-exception :key :system-defined :code code
                                   :phrase (format nil "Unrecognized system exception code ~D" code))))))
