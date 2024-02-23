(in-package #:forth)

(defconstant +char-size+ 1)

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
