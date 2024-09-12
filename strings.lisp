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

;;; In 2016, the Forth standardization committee adopted a proposal that characters occupy
;;; one address unit (i.e., byte) in memory
(defconstant +char-size+ 1)
(defconstant +longest-counted-string+ (1- (dpb 1 (byte 1 8) 0)))

(declaim (inline extract-char))
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun extract-char (value)
  (declare (fixnum value) (optimize (speed 3) (safety 0)))
  (ldb (byte 8 0) value))
)

;;;--- TODO: Do we need to error check here?
(declaim (inline forth-char))
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun forth-char (native-char)
  (declare (type character native-char) (optimize (speed 3) (safety 0)))
  (extract-char (char-code native-char)))
)

(declaim (inline native-char))
(defun native-char (forth-char)
  (declare (type (integer 0 255) forth-char) (optimize (speed 3) (safety 0)))
  (code-char forth-char))

(defconstant +forth-char-space+ #.(forth-char #\Space))


;;;---*** TODO: Try to find a more efficient way to do these conversions

(defun native-into-forth-string (native-string forth-memory offset)
  (declare (type string native-string) (type (simple-array (unsigned-byte 8) (*)) forth-memory) (fixnum offset)
           (optimize (speed 3) (safety 0)))
  (loop with length fixnum = (the fixnum (length native-string))
        for i fixnum below length
        do (setf (aref forth-memory (the fixnum (+ offset i))) (forth-char (aref native-string i))))
  (length native-string))

(defun native-into-forth-counted-string (native-string forth-memory offset)
  (declare (type string native-string) (type (simple-array (unsigned-byte 8) (*)) forth-memory) (fixnum offset)
           (optimize (speed 3) (safety 0)))
  (unless (<= (length native-string) +longest-counted-string+)
    (forth-exception :parse-string-overflow))
  ;; Length of a counted string is always a single byte regardless of character size
  (setf (aref forth-memory offset) (the fixnum (length native-string)))
  (native-into-forth-string native-string forth-memory (the fixnum (1+ offset))))

(defun forth-string-to-native (forth-memory offset length)
  (declare (type (simple-array (unsigned-byte 8) (*)) forth-memory) (fixnum offset) (fixnum length)
           (optimize (speed 3) (safety 0)))
  (let ((string (make-string length)))
    (declare (type simple-string string))
    (loop for i fixnum below length
          do (setf (aref string i) (native-char (aref forth-memory (the fixnum (+ offset i))))))
    string))

(defun forth-counted-string-to-native (forth-memory offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) forth-memory) (fixnum offset)
           (optimize (speed 3) (safety 0)))
  ;; Length of a counted string is always a single byte regardless of character size
  (forth-string-to-native forth-memory (the fixnum (1+ offset)) (aref forth-memory offset)))


;;; REPLACES/SUBSTITUTE support

(defconstant +native-char-escape+ #\%)
(defconstant +forth-char-escape+ #.(forth-char #\%))

(defclass replacements ()
  ;; Forth 2012 doesn't state whether or not replacement processing is case insensitive
  ;;  We'll presume it should be case insensitive and, consequently, use an EQUALP hash table
  ((table :initform (make-hash-table :test #'equalp)))
  )

(defmethod save-to-template ((replacements replacements))
  (with-slots (table) replacements
    (let ((saved-table (make-hash-table :size (hash-table-count table) :test #'equalp)))
      (maphash #'(lambda (name substitution) (setf (gethash name saved-table) substitution)) table)
      saved-table)))

(defmethod load-from-template ((replacements replacements) template fs)
  (declare (ignore fs))
  (with-slots (table) replacements
    (clrhash table)
    (maphash #'(lambda (name substitution) (setf (gethash name table) substitution)) template))
  nil)

(defmethod register-replacement ((replacements replacements) name substitution)
  (with-slots (table) replacements
    (unless (null (position +native-char-escape+ name :test #'eql))
      (forth-exception :replaces-exception "Substition name cannot include escape character (~A)" +native-char-escape+))
    (setf (gethash name table) substitution)))

(defmethod perform-substitute ((replacements replacements) input)
  (with-slots (table) replacements
    (let ((output (make-array (length input) :element-type 'character :fill-pointer 0 :adjustable t))
          (n-replacements 0))
      (flet ((add-substring (start end)
               (let* ((output-start (fill-pointer output))
                      (output-end (+ output-start (- end start))))
                 (if (> output-end (array-total-size output))
                     (adjust-array output output-end :fill-pointer output-end)
                     (setf (fill-pointer output) output-end))
                 (replace output input :start1 output-start :end1 output-end :start2 start :end2 end)))
             (add-substitution (substitution)
               (let* ((output-start (fill-pointer output))
                      (output-end (+ output-start (length substitution))))
                 (if (> output-end (array-total-size output))
                     (adjust-array output output-end :fill-pointer output-end)
                     (setf (fill-pointer output) output-end))
                 (replace output substitution :start1 output-start :end1 output-end))))
        (loop with start = 0
              with end = (length input)
              while (< start end)
              do (let ((next (position +native-char-escape+ input :start start :test #'eql)))
                   (if next
                       (let ((name-end nil))
                         (when (> next start)
                           (add-substring start next))
                         (cond ((= next (1- end))
                                (vector-push-extend +native-char-escape+ output)
                                (setf start end))
                               ((setf name-end (position +native-char-escape+ input :start (1+ next) :test #'eql))
                                (if (= name-end (1+ next))
                                    (vector-push +native-char-escape+ output)
                                    (let* ((name (subseq input (1+ next) name-end))
                                           (substitution (gethash name table)))
                                      (if substitution
                                          (progn
                                            (add-substitution substitution)
                                            (incf n-replacements))
                                          ;; No substition with that name -- Treat as ordinary text
                                          (add-substring next (1+ name-end)))))
                                (setf start (1+ name-end)))
                               (t
                                ;; Treat a single escape character in middle of the string as ordinary text
                                (add-substring next end)
                                (setf start end))))
                       (when (< start end)
                         (add-substring start end)
                         (setf start end)))))
        (values output n-replacements)))))
