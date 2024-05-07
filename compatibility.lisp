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


;;; MAKE-PIPED-STREAMS

#+CCL
(defun make-piped-streams (&key (element-type 'character) (external-format :default))
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
                                     :basic t
                                     :encoding encoding
                                     :line-termination line-termination
                                     :auto-close t)))
        (ccl::add-auto-flush-stream os)
        (values is os)))))

#+SBCL
(defun make-piped-streams (&key (element-type 'character) (external-format :default))
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
(defun make-piped-streams (&key (element-type 'character) (external-format :default))
  (declare (ignore element-type external-format))
  (error "NYI: ~S" 'make-piped-streams))
