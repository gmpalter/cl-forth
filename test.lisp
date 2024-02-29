(in-package #:forth)

(defvar *fs* nil)

(defun run ()
  (write-line "CL-Forth 0.9")
  (let ((fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (toplevel fs)))

(defun hex (n)
  (format nil "~X" n))

(defun word-counts ()
  (dolist (wl (word-lists-search-order (slot-value *fs* 'word-lists)))
    (format t "~&Word List \"~A\": ~D word~:P~%" (dictionary-name wl) (hash-table-count (dictionary-words wl)))))
