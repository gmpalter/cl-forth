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

;;; Resolve minor differences between supported Lisp implementations

;;; SBCL's DEFCONSTANT will complain when the constant is a bytespec (i.e., (BYTE ...)) as it represents bytespecs
;;; as a CONS which are not EQL when the load-time value tries to replace the compile-time value. (SBCL is strictly
;;; adhering to the ANSI CL specification in this case.)

#+SBCL
(defmacro define-constant (name value &optional docstring)
  (let ((value-var (gensym)))
    `(defconstant ,name
       (let ((,value-var ,value))
         (if (and (boundp ',name) (equal (symbol-value ',name) ,value-var))
             (symbol-value ',name)
             ,value-var))
       ,@(when docstring (list docstring)))))

#-SBCL
(defmacro define-constant (name value &optional docstring)
  `(defconstant ,name ,value ,@(when docstring (list docstring))))


;;; ----------------------------------------------------------------------------
;;; NAMED-LAMBDA

#+CCL
(defmacro named-lambda (name arglist &body body)
  `(ccl:nfunction ,name (lambda ,arglist ,@body)))

;;; SBCL provides NAMED-LAMBDA natively

#+LispWorks
(defmacro named-lambda (name arglist &body body)
  `(lambda ,arglist
     (declare (hcl:lambda-name ,name))
     ,@body))

#+ECL
(defmacro named-lambda (name arglist &body body)
  `(ext:lambda-block ,name ,arglist ,@body))


;;; ----------------------------------------------------------------------------
;;; WHITESPACEP

;;; CCL provides WHITESPACEP natively

#-CCL
(declaim (inline whitespacep))

#+SBCL
(defun whitespacep (ch) (sb-unicode:whitespace-p ch))

#+LispWorks
(defun whitespacep (ch) (lw:whitespace-char-p ch))

#+ECL
(defun whitespacep (ch) (or (eql ch #\Space) (eql ch #\Tab)))


;;; ----------------------------------------------------------------------------
;;; SET-STREAM-LENGTH

#+CCL
(defun set-stream-length (stream new-length)
  (ccl::stream-length stream new-length))

#+SBCL
(defun set-stream-length (stream new-length)
  (if (sb-sys:fd-stream-p stream)
      (let ((fd (sb-sys:fd-stream-fd stream)))
        (sb-posix:ftruncate fd new-length))
      (error 'file-error :pathname (pathname stream))))

#+(or LispWorks ECL)
(defun set-stream-length (stream new-length)
  (declare (ignore new-length))
  (error 'file-error :pathname (pathname stream)))


;;; ----------------------------------------------------------------------------
;;; MAKE-PIPED-STREAMS

#+CCL
(defclass unbuffered-fd-character-output-stream (ccl::fd-character-output-stream)
  ())

#+CCL
(defmethod ccl:stream-write-char :after ((st unbuffered-fd-character-output-stream) char)
  (declare (ignore char))
  (force-output st))

#+CCL
(defmethod ccl:stream-write-string :after ((st unbuffered-fd-character-output-stream) string &optional start end)
  (declare (ignore string start end))
  (force-output st))

#+CCL
(defmethod ccl:stream-terpri :after ((st unbuffered-fd-character-output-stream))
  (force-output st))

#+CCL
;; Used by WRITE-SEQUENCE
(defmethod ccl:stream-write-vector :after ((st unbuffered-fd-character-output-stream) vector start end)
  (declare (ignore vector start end))
  (force-output st))

#+CCL
(defun make-piped-streams (&key name (element-type 'character) (external-format :default))
  (declare (ignore name))
  (let* ((char-p (or (eq element-type 'character) (subtypep element-type 'character)))
         (real-external-format (when char-p
                                 (ccl::normalize-external-format :pipe external-format)))
         (encoding (when char-p (ccl:external-format-character-encoding real-external-format)))
         (line-termination (when char-p (ccl:external-format-line-termination real-external-format))))
    (multiple-value-bind (read-fd write-fd) (ccl::pipe)
      (let ((is (ccl::make-fd-stream read-fd
                                     :direction :input
                                     :interactive t
                                     :element-type element-type
                                     :sharing :lock
                                     :basic t
                                     :encoding encoding
                                     :line-termination line-termination
                                     :auto-close t))
            (os (ccl::make-fd-stream write-fd
                                     :direction :output
                                     :interactive t
                                     :element-type element-type
                                     :sharing :lock
                                     :basic nil
                                     :encoding encoding
                                     :line-termination line-termination
                                     :auto-close t)))
        (change-class os 'unbuffered-fd-character-output-stream)
        (values is os)))))

#+SBCL
(defun make-piped-streams (&key name (element-type 'character) (external-format :default))
  (declare (ignore name))
  (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
    (let ((is (sb-impl::make-fd-stream read-fd
                                       :input t
                                       :element-type element-type
                                       :buffering :line
                                       :external-format external-format
                                       :auto-close t))
          (os (sb-impl::make-fd-stream write-fd
                                       :output t
                                       :element-type element-type
                                       :buffering :line
                                       :external-format external-format
                                       :auto-close t)))
      (values is os))))

#+LispWorks
(defun make-piped-streams (&key name (element-type 'character) (external-format :default))
  (assert (eq element-type 'character) () "~S only supports ~S ~S, not ~S"
          'make-piped-streams :element-type 'character element-type)
  (assert (eq external-format :default) () "~S only supports ~S ~S, not ~S"
          'make-piped-streams :external-format :default external-format)
  (let ((buffer (make-in-memory-buffer +default-in-memory-buffer-size+)))
    (values (make-in-memory-character-input-stream buffer name)
            (make-in-memory-character-output-stream buffer name))))

#+ECL
(defun make-piped-streams (&key name (element-type 'character) (external-format :default))
  (declare (ignore name))
  (assert (eq element-type 'character) () "~S only supports ~S ~S, not ~S"
          'make-piped-streams :element-type 'character element-type)
  (assert (eq external-format :default) () "~S only supports ~S ~S, not ~S"
          'make-piped-streams :external-format :default external-format)
  (let ((pipe (ext:make-pipe)))
    (values (two-way-steram-input-stream pipe)
            (two-way-steram-output-stream pipe))))


;;; ----------------------------------------------------------------------------
;;; PROCESS-RUN-FUNCTION, PROCESS-WAIT

;;; CCL provides PROCESS-RUN-FUNCTION and PROCESS-WAIT natively

#+SBCL
(defun process-run-function (name-or-keywords function &rest args)
  (let ((name (if (listp name-or-keywords)
                  (destructuring-bind (&key (name "Anonymous") &allow-other-keys)
                      name-or-keywords
                    name)
                  name-or-keywords)))
    (sb-thread:make-thread function :name name :arguments args)))

#+SBCL
(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args) (ignore whostate))
  (loop until (apply function args)
        do (sleep 0.001)))

#+LispWorks
(defun process-run-function (name-or-keywords function &rest args)
  (multiple-value-bind (name priority)
      (if (listp name-or-keywords)
          (destructuring-bind (&key (name "Anonymous") (priority mp:*default-process-priority*) &allow-other-keys)
              name-or-keywords
            (values name priority))
          (values name-or-keywords mp:*default-process-priority*))
    (apply #'mp:process-run-function name `(:priority ,priority) function args)))

;;; LispWorks provides PROCESS-WAIT natively

#+ECL
(defun process-run-function (name-or-keywords function &rest args)
  (let ((name (if (listp name-or-keywords)
                  (destructuring-bind (&key (name "Anonymous") &allow-other-keys)
                      name-or-keywords
                    name)
                  name-or-keywords)))
    (apply #'mp:process-run-function name function args)))

#+ECL
(defun process-wait (whostate function &rest args)
  (declare (dynamic-extent args) (ignore whostate))
  (loop until (apply function args)
        do (sleep 0.001)))


;;; ----------------------------------------------------------------------------
;;; ADD/REMOVE-AUTO-FLUSH-STREAM

;;; CCL provides ADD/REMOVE-AUTO-FLUSH-STREAM natively

#-CCL
(defun add-auto-flush-stream (stream)
  (declare (ignore stream))
  nil)

#-CCL
(defun remove-auto-flush-stream (stream)
  (declare (ignore stream))
  nil)


;;; ----------------------------------------------------------------------------
;;; ADDRESS-POINTER, POINTER-ADDRESS, NULL-POINTER-P, %ADDRESS-OF

(declaim (inline address-pointer pointer-address null-pointer-p null-pointer %address-of))

(defun address-pointer (address)
  (cffi:make-pointer address))

(defun pointer-address (pointer)
  (cffi:pointer-address pointer))

(defun null-pointer-p (pointer)
  (cffi:null-pointer-p pointer))

(defun null-pointer ()
  (cffi:null-pointer))

#+CCL
(defun %address-of (object)
  ;; CCL's %ADDRESS-OF doesn't strip the tag from the address
  (logand (ccl:%address-of object) (lognot 7)))

#+SBCL
(defun %address-of (object)
  ;; The OBJECT should be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) as that's how CL-Forth represents memory
  (sb-sys:sap-int (sb-sys:vector-sap object)))

#+LispWorks
(defun %address-of (object)
  ;; The OBJECT should be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) as that's how CL-Forth represents memory
  ;; LispWorks returns the address of the object's header word, skip to the actual data
  (+ (system:object-address object) 8))

#+ECL
(defun %address-of (object)
  ;; The OBJECT should be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) as that's how CL-Forth represents memory
  ;; ECL array objects contain a pointer to the actual data and SI:MAKE-FOREIGN-DATA-FROM-ARRAY extracts that pointer
  (pointer-address (si:make-foreign-data-from-array object)))


;;; ----------------------------------------------------------------------------
;;; OBJECT-SIZE

(declaim (inline object-size))

#+CCL
(defun object-size (object)
  (ccl:object-direct-size object))

#+SBCL
(defun object-size (object)
  (sb-ext:primitive-object-size object))

#+LispWorks
(defun object-size (object)
  (hcl:find-object-size object))

#+ECL
(defun object-size (object)
  (declare (ignore object))
  0)


;;; ----------------------------------------------------------------------------
;;; PARSE-BODY

(defun parse-body (body &key documentation whole) ;; from alexandria
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (cond ((eq documentation :multiple)
                (push (pop body) doc))
               (doc
                (error "Too many documentation strings in ~S." (or whole body)))
               (t
                (setf doc (pop body))))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) (if (eq documentation :multiple) (nreverse doc) doc))))
