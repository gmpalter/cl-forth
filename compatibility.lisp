(in-package #:forth)

;;; CCL, SBCL, and LispWorks have some minor differences which are resolved here

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


;;; WHITESPACEP

;;; CCL provides WHITESPACEP natively

#-CCL
(declaim (inline whitespacep))

#+SBCL
(defun whitespacep (ch) (sb-unicode:whitespace-p ch))

#+LispWorks
(defun whitespacep (ch) (lw:whitespace-char-p ch))


;;; SET-STREAM-LENGTH

#+CCL
(defun set-stream-length (stream new-length)
  (ccl::stream-length stream new-length))

#+SBCL
(defun set-stream-length (stream new-length)
  (declare (ignore new-length))
  (error 'file-error :pathname (pathname stream)))

#+LispWorks
(defun set-stream-length (stream new-length)
  (declare (ignore new-length))
  (error 'file-error :pathname (pathname stream)))
