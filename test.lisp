(in-package #:forth)

(defvar *fs* nil)

(defun run ()
  (let ((me (asdf:find-system '#:cl-forth)))
    (format t "~&~A ~A~%" (asdf:system-long-name me) (asdf:system-version me)))
  (let ((fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (forth-toplevel fs)))

(setf (symbol-function 'cl-user::run) (symbol-function 'run))

(defun run-2012-tests (&optional verbose?)
  (let ((me (asdf:find-system '#:cl-forth)))
    (format t "~&~A ~A~%" (asdf:system-long-name me) (asdf:system-version me)))
  (let ((*default-pathname-defaults* (probe-file "tests/src/"))
        (fs (make-instance 'forth-system)))
    (setf *fs* fs)
    (with-input-from-string (text #.(format nil "The quick brown fox jumped over the lazy red dog.~%"))
      (let ((*standard-input* (make-concatenated-stream text *standard-input*)))
        (forth-toplevel fs :evaluate (format nil "~@[ VERBOSE~] WARNING OFF S\" runtests.fth\" INCLUDED" verbose?))))))

(setf (symbol-function 'cl-user::run-2012-tests) (symbol-function 'run-2012-tests))
