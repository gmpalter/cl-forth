;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:forth)

;;; Highly performant character streams using an in-memory buffer

(defparameter +default-in-memory-buffer-size+ 65536)

(defstruct (in-memory-buffer (:conc-name imb-)
                             (:constructor %make-in-memory-buffer)
                             (:print-function %print-in-memory-buffer))
  buffer
  (size 0 :type fixnum)
  (put 0 :type fixnum)
  (take 0 :type fixnum)
  (closed? nil))

(defun make-in-memory-buffer (size)
  (%make-in-memory-buffer :buffer (make-array size :element-type 'character :initial-element #\Null) :size size))

(defun %print-in-memory-buffer (imb stream depth)
  (if (zerop depth)
      (print-unreadable-object (imb stream :type t :identity t)
        (format stream "size=~D, put=~D, take=~D~@[, closed~]" (imb-size imb) (imb-put imb) (imb-take imb) (imb-closed? imb)))
      (format stream "size=~D, put=~D, take=~D~@[, closed~]" (imb-size imb) (imb-put imb) (imb-take imb) (imb-closed? imb))))

(declaim (inline imb-free))
(defun imb-free (imb)
  (declare (type in-memory-buffer imb)
           (optimize (speed 3) (safety 0)))
  (let ((put (imb-put imb))
        (take (imb-take imb))
        (size (imb-size imb)))
    (cond ((= put take)
           size)
          ((> take put)
           (the fixnum (- (the fixnum (- take put)) 1)))
          (t
           (the fixnum (- (the fixnum (- (the fixnum (- take put)) 1)) (the fixnum (- size))))))))

(declaim (inline imb-empty?))
(defun imb-empty? (imb)
  (declare (type in-memory-buffer imb)
           (optimize (speed 3) (safety 0)))
  (let ((size (imb-size imb)))
    (= size (the fixnum (imb-free imb)))))

(declaim (inline imb-full?))
(defun imb-full? (imb)
  (declare (type in-memory-buffer imb)
           (optimize (speed 3) (safety 0)))
  (let ((put (imb-put imb))
        (take (imb-take imb))
        (size (imb-size imb)))
    (declare (fixnum put take size))
    (if (> take put)
        (zerop (the fixnum (- (the fixnum (- take put)) 1)))
        (zerop (the fixnum (- (the fixnum (- (the fixnum (- take put)) 1)) (the fixnum (- size))))))))

(declaim (inline imb-put-char))
(defun imb-put-char (imb char &optional (wait? t))
  (declare (type in-memory-buffer imb)
           (optimize (speed 3) (safety 0)))
  (when (imb-full? imb)
    (if wait?
        (ccl:process-wait "Buffer Put" #'(lambda (imb) (not (imb-full? imb))) imb)
        (return-from imb-put-char nil)))
  (let ((put (imb-put imb))
        (size (imb-size imb)))
    (declare (fixnum put size))
    (setf (aref (the (simple-array character (*)) (imb-buffer imb)) put) char)
    (incf put)
    (when (>= put size)
      (setf put 0))
    ;;---*** TODO: Should we do something else here?
    (loop while (= put (the fixnum (imb-take imb))))
    (setf (imb-put imb) put)
    char))

(declaim (inline imb-put-string))
(defun imb-put-string (imb string start end)
  (declare (type in-memory-buffer imb)
           (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (loop with start fixnum = start
        with length fixnum = (the fixnum (- end start))
        with put fixnum = (imb-put imb)
        with size fixnum = (imb-size imb)
        while (plusp length)
        for chunk fixnum = (min (the fixnum (min length (the fixnum (imb-free imb)))) (the fixnum (- size put)))
        when (imb-full? imb)
          do (ccl:process-wait "Buffer Put String" #'(lambda (imb) (not (imb-empty? imb))) imb)
        do (replace (imb-buffer imb) string :start1 put :end1 (the fixnum (+ put chunk))
                                              :start2 start :end2 (the fixnum (+ start chunk)))
             (setf put (the fixnum (+ put  chunk)))
             (when (>= put size)
               (setf put 0))
             ;;---*** TODO: Should we do something else here?
             (loop while (= put (the fixnum (imb-take imb))))
             (setf (imb-put imb) put
                   start (the fixnum (+ start chunk))
                   length (the fixnum (- length chunk))))
  string)

(declaim (inline imb-take-char))
(defun imb-take-char (imb &optional (wait? t))
  (declare (type in-memory-buffer imb)
           (optimize (speed 3) (safety 0)))
  (when (imb-empty? imb)
    (if wait?
        (ccl:process-wait "Buffer Take" #'(lambda (imb) (not (imb-empty? imb))) imb)
        (return-from imb-take-char nil)))
  (let* ((take (imb-take imb))
         (size (imb-size imb))
         (char (aref (the (simple-array character (*)) (imb-buffer imb)) take)))
    (declare (fixnum take size))
    (incf take)
    (when (>= take size)
      (setf take 0))
    (setf (imb-take imb) take)
    char))


;;;

(defclass in-memory-character-stream (ccl:fundamental-character-stream)
  ((name :initarg :name :initform nil)
   (buffer :accessor imcs-buffer :initarg :buffer :initform nil))
  )

(defmethod initialize-instance :after ((st in-memory-character-stream) &key &allow-other-keys)
  (assert (in-memory-buffer-p (imcs-buffer st)) () "~S must be specified for ~S" :buffer (class-name (class-of st))))

(defmethod print-object ((st in-memory-character-stream) stream)
  (with-slots (name buffer) st
    (print-unreadable-object (st stream :type t :identity t)
      (format stream "~@[~A: ~]~S" name buffer))))

(defmethod open-stream-p ((st in-memory-character-stream))
  (not (imb-closed? (imcs-buffer st))))

(defmethod close ((st in-memory-character-stream) &key abort)
  (declare (ignore abort))
  (with-slots (buffer) st
    (prog1
        (not (imb-closed? buffer))
      (setf (imb-closed? buffer) t))))

(defmacro with-imcs-buffer ((st &key eof?) &body body)
  `(with-slots (buffer) ,st
     (when (and (imb-closed? buffer) (not ,eof?))
       (ccl::stream-is-closed ,st))
     ,@body))


;;;

(defclass in-memory-character-input-stream (ccl:fundamental-character-input-stream in-memory-character-stream)
  ((unread-char :initform nil))
  )

(defmethod ccl:stream-read-char ((st in-memory-character-input-stream))
  (with-imcs-buffer (st)
    (with-slots (unread-char) st
      (if unread-char
          (shiftf unread-char nil)
          (imb-take-char buffer)))))

(defmethod ccl:stream-unread-char ((st in-memory-character-input-stream) char)
  (with-slots (unread-char) st
    (if unread-char
        (error "Two UNREAD-CHARs without intervening READ-CHAR on ~s" st)
        (setf unread-char char))))

(defmethod ccl:stream-listen ((st in-memory-character-input-stream))
  (with-imcs-buffer (st :eof? t)
    (if (imb-closed? buffer)
        :eof
        (not (imb-empty? buffer)))))

(defun make-in-memory-character-input-stream (in-memory-buffer &optional name)
  (make-instance 'in-memory-character-input-stream :buffer in-memory-buffer :name name))


;;;

(defclass in-memory-character-output-stream (ccl:fundamental-character-output-stream in-memory-character-stream)
  ((column :initform 0 :reader ccl:stream-line-column))
  )

(defmethod ccl:stream-write-char ((st in-memory-character-output-stream) char)
  (with-imcs-buffer (st)
    (with-slots (column) st
      (imb-put-char buffer char)
      (if (eql char #\Newline)
          (setf column 0)
          (incf column))
      char)))

(defmethod ccl:stream-write-string ((st in-memory-character-output-stream) string &optional start end)
  (let ((start (or start 0))
        (end (or end (length string))))
    (with-imcs-buffer (st)
      (with-slots (column) st
        (imb-put-string buffer string start end)
        (let ((last-newline (position #\Newline string :start start :end end :from-end t)))
          (if last-newline
              (setf column (- end last-newline))
              (incf column (- end start))))))
    string))

(defmethod ccl:stream-force-output ((st in-memory-character-output-stream))
  nil)

(defun make-in-memory-character-output-stream (in-memory-buffer &optional name)
  (make-instance 'in-memory-character-output-stream :buffer in-memory-buffer :name name))
