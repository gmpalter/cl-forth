(in-package #:forth)

(defconstant +char-size+ 1)
(defconstant +longest-counted-string+ (1- (dpb 1 (byte 1 8) 0)))

(declaim (inline extract-char))
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun extract-char (value)
  (ldb (byte 8 0) value))
)

;;;--- TODO: Do we need to error check here?
(declaim (inline forth-char))
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun forth-char (native-char)
  (extract-char (char-code native-char)))
)

(declaim (inline native-char))
(defun native-char (forth-char)
  (code-char forth-char))

(defconstant +forth-char-space+ #.(forth-char #\Space))


;;;---*** TODO: Try to find a more efficient way to do these conversions

(defun native-into-forth-string (native-string forth-memory offset)
  (loop for i below (length native-string)
        do (setf (aref forth-memory (+ offset i)) (forth-char (aref native-string i))))
  (length native-string))

(defun native-into-forth-counted-string (native-string forth-memory offset)
  (unless (<= (length native-string) +longest-counted-string+)
    (forth-error :parse-string-overflow))
  (setf (aref forth-memory offset) (length native-string))
  (forth-string-into native-string forth-memory (1+ offset)))

(defun forth-string-to-native (forth-memory offset length)
  (let ((string (make-string length)))
    (loop for i below length
          do (setf (aref string i) (native-char (aref forth-memory (+ offset i)))))
    string))

(defun forth-counted-string-to-native (forth-memory offset)
  (native-string forth-memory (1+ offset) (aref forth-memory offset)))
