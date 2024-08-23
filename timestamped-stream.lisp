;;; -*- Syntax: Common-Lisp; Base: 10 -*-

(in-package #:forth)

;;; Wrapper stream which adds a timestamp to each line of output

(defclass timestamped-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((stream :accessor timestamped-stream-stream :initarg :stream :initform nil)
   (last-char :initform #\Newline))
  )

#+TODO
;;; SBCL and LispWorks don't define CHARACTER-OUTPUT-STREAM
(defmethod initialize-instance :after ((ts timestamped-stream) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (check-type (timestamped-stream-stream ts) (or character-output-stream synonym-stream))
  )

(defmethod print-object ((ts timestamped-stream) out)
  (print-unreadable-object (ts out :type t :identity t)
    (print-object (timestamped-stream-stream ts) out)))

(declaim (inline timestamp-string))
(defun timestamp-string ()
  (multiple-value-bind (secs us)
      (floor (get-internal-real-time) internal-time-units-per-second)
    (format nil "~6,'0D.~6,'0D: " secs us)))

(defmethod trivial-gray-streams:stream-write-char ((ts timestamped-stream) char)
  (with-slots (stream last-char) ts
    (when (eql (shiftf last-char char) #\Newline)
      (fresh-line stream)
      (write-string (timestamp-string) stream))
    (write-char char stream)))

(defmethod trivial-gray-streams:stream-write-string ((ts timestamped-stream) string &optional (start 0) (end (length string)))
  (setf start (or start 0)
        end (or end (length string)))
  (with-slots (stream last-char) ts
    (when (eql (shiftf last-char (if (plusp end) (aref string (1- end)) #\Nul)) #\Newline)
      (fresh-line stream)
      (write-string (timestamp-string) stream))
    (loop with start = start
          while (< start end)
          for position = (position #\Newline string :start start :end end)
          do (write-string string stream :start start :end (or position end))
             (when position
               (terpri stream)
               (write-string (timestamp-string) stream))
             (setf start (1+ (or position end)))))
  string)

(defmethod trivial-gray-streams:stream-line-column ((ts timestamped-stream))
  #+SBCL (sb-kernel:charpos (timestamped-stream-stream ts))
  #-SBCL (trivial-gray-streams:stream-line-column (timestamped-stream-stream ts)))

(defmethod trivial-gray-streams:stream-force-output ((ts timestamped-stream))
  (force-output (timestamped-stream-stream ts)))

(defmethod trivial-gray-streams:stream-finish-output ((ts timestamped-stream))
  (finish-output (timestamped-stream-stream ts)))

(defmethod close ((ts timestamped-stream) &key abort)
  (declare (ignore abort))
  (when (open-stream-p (timestamped-stream-stream ts))
    (close (timestamped-stream-stream ts))
    t))

(defun make-timestamped-stream (output-stream)
  (make-instance 'timestamped-stream :stream output-stream))
