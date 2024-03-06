(in-package #:forth)

(defvar *fs* nil)

(defun run ()
  (write-line "CL-Forth 0.9")
  (let ((fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (toplevel fs)))

(defun run-tests (&optional verbose?)
  (write-line "CL-Forth 0.9")
  (let ((*default-pathname-defaults* (probe-file "../cl-forth-tests/"))
        (fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (with-input-from-string (text #.(format nil "The quick brown fox jumped over the lazy red dog.~%"))
      (let ((*standard-input* (make-concatenated-stream text *standard-input*)))
        (toplevel fs :evaluate (format nil "~@[ VERBOSE~] INCLUDE cl-forth-tests" verbose?))))))

(defun run-2012-tests (&optional verbose?)
  (write-line "CL-Forth 0.9")
  (let ((*default-pathname-defaults* (probe-file "../forth2012-test-suite/src/"))
        (fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (with-input-from-string (text #.(format nil "The quick brown fox jumped over the lazy red dog.~%"))
      (let ((*standard-input* (make-concatenated-stream text *standard-input*)))
        (toplevel fs :evaluate (format nil "~@[ VERBOSE~] WARNING OFF S\" runtests.fth\" INCLUDED" verbose?))))))

(defun hex (n)
  (format nil "~X" n))

(defun word-counts ()
  (dolist (wl (word-lists-search-order (slot-value *fs* 'word-lists)))
    (format t "~&Word List \"~A\": ~D word~:P~%" (dictionary-name wl) (hash-table-count (dictionary-words wl)))))
