(in-package #:forth)

(defvar *fs* nil)

(defun run ()
  (let ((me (asdf:find-system '#:cl-forth)))
    (format t "~&~A ~A~%" (asdf:system-long-name me) (asdf:system-version me)))
  (let ((fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (toplevel fs)))

(setf (symbol-function 'cl-user::run) (symbol-function 'run))

(defun run-2012-tests (&optional verbose?)
  (let ((me (asdf:find-system '#:cl-forth)))
    (format t "~&~A ~A~%" (asdf:system-long-name me) (asdf:system-version me)))
  (let ((*default-pathname-defaults* (probe-file "tests/src/"))
        (fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (with-input-from-string (text #.(format nil "The quick brown fox jumped over the lazy red dog.~%"))
      (let ((*standard-input* (make-concatenated-stream text *standard-input*)))
        (toplevel fs :evaluate (format nil "~@[ VERBOSE~] WARNING OFF S\" runtests.fth\" INCLUDED" verbose?))))))

(setf (symbol-function 'cl-user::run-2012-tests) (symbol-function 'run-2012-tests))

(defun run-ascent ()
  (let ((me (asdf:find-system '#:ascent-forth)))
    (format t "~&~A ~A~%" (asdf:system-long-name me) (asdf:system-version me)))
  (let ((fs (forth-asdf-support::fas-forth-system (asdf:find-system '#:ascent-forth))))
    (setf *fs* fs)
    (toplevel fs)))

(setf (symbol-function 'cl-user::run-ascent) (symbol-function 'run-ascent))

(defun hex (n)
  (format nil "~X" n))

(defun word-counts ()
  (dolist (wl (word-lists-search-order (slot-value *fs* 'word-lists)))
    (format t "~&Word List \"~A\": ~D word~:P~%" (dictionary-name wl) (hash-table-count (dictionary-words wl)))))
