(in-package #:forth)

(defvar *fs* nil)

(defun run ()
  (write-line "CL-Forth 0.9")
  (let ((fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (toplevel fs)))

(defun hex (n)
  (format nil "~X" n))
