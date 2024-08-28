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

(defconstant +cell-size+ 8)
(defconstant +byte-to-cell-shift+ -3)

(defconstant +byte-to-double-byte-shift+ -1)
(defconstant +byte-to-quad-byte-shift+ -2)

(defconstant +single-float-cell-size+ 4)
(defconstant +double-float-cell-size+ 8)
;;; CL-Forth uses double precision floating point as its internal representation of float values
(defconstant +native-float-cell-size+ +double-float-cell-size+)

(defconstant +address-prefix-size+ 8)
(defconstant +address-address-size+ (- (integer-length most-positive-fixnum) +address-prefix-size+))
(define-constant +address-prefix-byte+ (byte +address-prefix-size+ +address-address-size+))
(define-constant +address-address-byte+ (byte +address-address-size+ 0))

(declaim (inline make-address))
(defun make-address (prefix address)
  (declare (type (integer 0 #xFF) prefix) (fixnum address)
           (optimize (speed 3) (safety 0)))
  #+CCL (logior (ash prefix (byte-position +address-prefix-byte+)) (logand address (dpb -1 +address-address-byte+ 0)))
  #-CCL (dpb prefix +address-prefix-byte+ address))

(declaim (inline address-prefix))
(defun address-prefix (address)
  (declare (fixnum address) (optimize (speed 3) (safety 0)))
  (ldb +address-prefix-byte+ address))

(declaim (inline address-address))
(defun address-address (address)
  (declare (fixnum address) (optimize (speed 3) (safety 0)))
  (ldb +address-address-byte+ address))


;;;

(defclass mspace ()
  ((prefix :accessor space-prefix :initarg :prefix :initform nil)
   (high-water-mark :accessor space-high-water-mark :initform 0))
  )

(defmethod print-object ((sp mspace) stream)
  (with-slots (prefix high-water-mark) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X, hwm=~D" prefix high-water-mark))))

(defgeneric space-reset (mspace))
(defgeneric save-space-state (mspace))

(defgeneric space-seal (mspace)
  (:method ((sp mspace)) nil))
(defgeneric space-unseal (mspace)
  (:method ((sp mspace)) nil))

(defgeneric space-save-to-template (mspace)
  (:method ((sp mspace)) :none))
(defgeneric space-load-from-template (mspace template)
  (:method ((sp mspace) template)
    (declare (ignore template))
    nil))

(defgeneric space-allocate (mspace n-bytes))
(defgeneric space-deallocate (mspace n-bytes))
(defgeneric space-unused (mspace)
  (:method ((sp mspace)) 0))
(defgeneric space-align (mspace &optional boundary))

(defgeneric space-state-slot-address (mspace slot)
  (:method (sp slot)
    (declare (ignore sp slot))
    nil))

(defgeneric cell-at (mspace address))
(defgeneric (setf cell-at) (value mspace address))

(defgeneric cell-unsigned-at (mspace address))
(defgeneric (setf cell-unsigned-at) (value mspace address))

(defgeneric quad-byte-at (mspace address))
(defgeneric (setf quad-byte-at) (value mspace address))

(defgeneric double-byte-at (mspace address))
(defgeneric (setf double-byte-at) (value mspace address))

(defgeneric byte-at (mspace address))
(defgeneric (setf byte-at) (value mspace address))

(defgeneric space-decode-address (mspace address &optional size-hint))
(defgeneric space-native-address (mspace foreign-address)
  (:method ((sp mspace) foreign-address)
    (declare (ignore foreign-address))
    nil))
(defgeneric space-foreign-address (mspace native-address)
  (:method ((sp mspace) native-address)
    (declare (ignore native-address))
    nil))
(defgeneric space-address-is-foreign? (mspace address)
  (:method ((sp mspace) address)
    (declare (ignore address))
    nil))

(defgeneric space-fill (mspace address count byte)
  (:method ((sp mspace) address count byte)
    (multiple-value-bind (data address size)
        (space-decode-address sp address count)
      (unless (<= (+ address count) size)
        (forth-exception :invalid-memory))
      (fill data byte :start address :end (+ address count)))))

(defgeneric space-copy (source-space source-address destination-space destination-address count)
  (:method ((ssp mspace) source-address (dsp mspace) destination-address count)
    (multiple-value-bind (source-data source-address source-size)
        (space-decode-address ssp source-address count)
      (multiple-value-bind (destination-data destination-address destination-size)
          (space-decode-address dsp destination-address count)
        (unless (<= (+ source-address count) source-size)
          (forth-exception :invalid-memory))
        (unless (<= (+ destination-address count) destination-size)
          (forth-exception :invalid-memory))
        (replace destination-data source-data :start1 destination-address :end1 (+ destination-address count)
                                              :start2 source-address :end2 (+ source-address count))))))


;;;

(defclass data-space (mspace)
  ((data :accessor data-space-data)
   (size :accessor data-space-size :initarg :size :initform 0)
   (data-foreign-address :initform nil)
   (saved-data :initform nil)
   (saved-high-water-mark :initform 0))
  )

(defmethod initialize-instance :after ((sp data-space) &key &allow-other-keys)
  (with-slots (data size) sp
    (assert (and (numberp size) (plusp size)) (size) "~S ~S must be a positive integer" 'data-space :size)
    (setf data (make-array size :element-type '(unsigned-byte 8) :initial-element 0))))

(defmethod print-object ((sp data-space) stream)
  (with-slots (prefix size high-water-mark) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X, size=~D, hwm=~D" prefix size high-water-mark))))

(defmethod space-reset ((sp data-space))
  (with-slots (data high-water-mark saved-high-water-mark saved-data) sp
    (setf high-water-mark saved-high-water-mark)
    (when (plusp high-water-mark)
      (replace data saved-data :end1 high-water-mark)))
  nil)

(defmethod save-space-state ((sp data-space))
  (with-slots (data high-water-mark saved-data saved-high-water-mark) sp
    (setf saved-high-water-mark high-water-mark
          saved-data nil)
    (when (plusp high-water-mark)
      (setf saved-data (make-array high-water-mark :element-type '(unsigned-byte 8) :initial-element 0))
      (replace saved-data data :end2 high-water-mark)))
  nil)

(defmethod space-save-to-template ((sp data-space))
  (with-slots (data high-water-mark) sp
    (let ((saved-data nil))
      (when (plusp high-water-mark)
        (setf saved-data (make-array high-water-mark :element-type '(unsigned-byte 8) :initial-element 0))
        (replace saved-data data :end2 high-water-mark))
      (list high-water-mark saved-data))))

(defmethod space-load-from-template ((sp data-space) template)
  (with-slots (data high-water-mark) sp
    (destructuring-bind (saved-high-water-mark saved-data) template
      (setf high-water-mark saved-high-water-mark)
      (when saved-data
        (replace data saved-data :end1 high-water-mark)))))

(defmethod space-allocate ((sp data-space) n-bytes)
  (with-slots (prefix size high-water-mark) sp
    (let ((address (make-address prefix high-water-mark)))
      (unless (<= (+ high-water-mark n-bytes) size)
        (forth-exception :data-space-overflow))
      (incf high-water-mark n-bytes)
      address)))

(defmethod space-deallocate ((sp data-space) n-bytes)
  (with-slots (high-water-mark) sp
    (decf high-water-mark (min high-water-mark n-bytes))))

(defmethod space-unused ((sp data-space))
  (with-slots (size high-water-mark) sp
    (- size high-water-mark)))

(defmethod space-align ((sp data-space) &optional (boundary +cell-size+))
  (with-slots (high-water-mark) sp
    (unless (zerop (mod high-water-mark boundary))
      (incf high-water-mark (- boundary (mod high-water-mark boundary))))))

(defmethod cell-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (cell-signed (aref data (ash address +byte-to-cell-shift+))))))

(defmethod (setf cell-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setf (aref data (ash address +byte-to-cell-shift+)) (cell-unsigned value)))))

(defmethod cell-unsigned-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (aref data (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-unsigned-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setf (aref data (ash address +byte-to-cell-shift+)) value))))

(defmethod quad-byte-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 32)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (quad-byte-signed (aref data (ash address +byte-to-quad-byte-shift+))))))

(defmethod (setf quad-byte-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 32)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setf (aref data (ash address +byte-to-quad-byte-shift+)) (quad-byte-unsigned value)))))

(defmethod double-byte-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 16)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (double-byte-signed (aref data (ash address +byte-to-double-byte-shift+))))))

(defmethod (setf double-byte-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 16)) data)
                      #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
      (setf (aref data (ash address +byte-to-double-byte-shift+)) (double-byte-unsigned value)))))

(defmethod byte-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (aref data address)))

(defmethod (setf byte-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (< address size)
      (forth-exception :invalid-memory))
    (setf (aref data address) (ldb (byte 8 0) value))))

(defmethod space-decode-address ((sp data-space) address &optional size-hint)
  (declare (ignore size-hint))
  (with-slots (data size) sp
    (values data address size)))

(defmethod space-native-address ((sp data-space) foreign-address)
  (with-slots (prefix data size data-foreign-address) sp
    (when (null data-foreign-address)
      (setf data-foreign-address (%address-of data)))
    (when (<= data-foreign-address foreign-address (+ data-foreign-address size -1))
      (make-address prefix (- foreign-address data-foreign-address)))))

(defmethod space-foreign-address ((sp data-space) native-address)
  (with-slots (data data-foreign-address) sp
    ;; For now, don't cache the foreign address as the data may move after a GC
    ;;---*** TODO: Can we pin the data? If the foreign code remembers an address, it might
    ;;             read and misinterpret whatever replaced the data or, worse, overwrite it.
    (when t ;;(null data-foreign-address)
      (setf data-foreign-address (%address-of data)))
    (+ data-foreign-address native-address)))

(defmethod space-fill ((sp data-space) address count byte)
  (with-slots (data size) sp
    (unless (<= (+ address count) size)
      (forth-exception :invalid-memory))
    (fill data byte :start address :end (+ address count))))

(defmethod space-copy ((ssp data-space) source-address (dsp data-space) destination-address count)
  (with-slots ((source-data data) (source-size size)) ssp
    (with-slots ((destination-data data) (destination-size size)) dsp
      (unless (<= (+ source-address count) source-size)
        (forth-exception :invalid-memory))
      (unless (<= (+ destination-address count) destination-size)
        (forth-exception :invalid-memory))
      (replace destination-data source-data :start1 destination-address :end1 (+ destination-address count)
                                            :start2 source-address :end2 (+ source-address count)))))


;;;

(defclass transient-data-space (data-space)
  ((active? :accessor transient-space-active? :initform nil))
  )

(defmethod space-reset :after ((sp transient-data-space))
  (with-slots (active?) sp
    (setf active? nil)))

(defmethod space-seal ((sp transient-data-space))
  (with-slots (active?) sp
    (setf active? nil)))
  
(defmethod space-unseal ((sp transient-data-space))
  (with-slots (active?) sp
    (setf active? t)))
  
(defmethod space-load-from-template :after ((sp transient-data-space) template)
  (declare (ignore template))
  (with-slots (active?) sp
    (setf active? nil)))

(defmethod (setf cell-at) :before (value (sp transient-data-space) address)
  (declare (ignore value address))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod (setf cell-unsigned-at) :before (value (sp transient-data-space) address)
  (declare (ignore value address))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod (setf quad-byte-at) :before (value (sp transient-data-space) address)
  (declare (ignore value address))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod (setf double-byte-at) :before (value (sp transient-data-space) address)
  (declare (ignore value address))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod (setf byte-at) :before (value (sp transient-data-space) address)
  (declare (ignore value address))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod space-fill :before ((sp transient-data-space) address count byte)
  (declare (ignore address count byte))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod space-copy :before ((ssp data-space) source-address (dsp transient-data-space) destination-address count)
  (declare (ignore source-address destination-address count))
  (with-slots (active?) dsp
    (unless active?
      (forth-exception :write-to-read-only-memory))))


;;;

(defclass pictured-buffer (transient-data-space)
  ((active? :accessor pictured-buffer-active? :initform nil)
   (used :initform 0))
  )

;;(defmethod initialize-instance :after ((pb pictured-buffer) &key &allow-other-keys)
;;  (with-slots (data size high-water-mark) pb
;;    ;; All memory operations check the high water mark to decide if the operation is valid
;;    ;; As we fill the buffer from the end towards the front, we have to claim its all "in use"
;;    (setf high-water-mark size)))

(defmethod print-object ((pb pictured-buffer) stream)
  (with-slots (prefix size used active?) pb
    (print-unreadable-object (pb stream :type t :identity t)
      (format stream "prefix=~2,'0X, size=~D, used=~D~@[, active~]" prefix size used active?))))

(defmethod space-reset ((pb pictured-buffer))
  (with-slots (active? used) pb
    (setf active? nil
          used 0)))

(defmethod space-save-to-template ((sp pictured-buffer))
  (with-slots (used) sp
    (let ((template (call-next-method)))
      (list template used))))

(defmethod space-load-from-template ((sp pictured-buffer) template)
  (with-slots (active? used) sp
    (destructuring-bind (base-template template-used) template
      (setf active? nil
            used template-used)
      (call-next-method sp base-template))))

(defmethod space-allocate ((pb pictured-buffer) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :pictured-output-overflow))

(defmethod start-pictured-buffer ((pb pictured-buffer))
  (with-slots (active? used) pb
    (when active?
      (forth-exception :recursive-pictured-output))
    (setf used 0
          active? t)))

(defmethod add-to-pictured-buffer ((pb pictured-buffer) forth-char)
  (with-slots (size used) pb
    (when (= used size)
      (forth-exception :pictured-output-overflow))
    (prog1
        (setf (byte-at pb (- size used 1)) forth-char)
      (incf used))))

(defmethod add-string-to-pictured-buffer ((pb pictured-buffer) memory address count)
  #+SBCL (declare (notinline memory-decode-address))
  (with-slots (data size used) pb
    (when (> (+ used count) size)
      (forth-exception :pictured-output-overflow))
    (multiple-value-bind (source-data offset)
        (memory-decode-address memory address count)
      (replace data source-data :start1 (- size used count) :end1 (- size used)
                                :start2 offset :end2 (+ offset count))
      (incf used count))))

(defmethod finish-pictured-buffer ((pb pictured-buffer))
  (with-slots (active? prefix size used) pb
    (setf active? nil)
    (let ((idx (- size used)))
      (values (make-address prefix idx) used))))


;;;

(defclass state-space (mspace)
  ((parent :initarg :parent :initform nil)
   (slots :initform nil))
  )

(defmethod initialize-instance :after ((sp state-space) &key &allow-other-keys)
  (with-slots (parent slots) sp
    (setf slots (map 'vector 'slot-definition-name (class-direct-slots (class-of parent))))))

(defmethod print-object ((sp state-space) stream)
  (with-slots (prefix parent) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X, parent=~S" prefix parent))))

(defmethod space-reset ((sp state-space))
  nil)

(defmethod save-space-state ((sp state-space))
  nil)

(defmethod space-allocate ((sp state-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :invalid-memory))

(defmethod space-align ((sp state-space) &optional (boundary +cell-size+))
  (declare (ignore boundary))
  nil)

(defmethod space-state-slot-address ((sp state-space) slot)
  (with-slots (prefix slots) sp
    (let ((index (position slot slots)))
      (when index
        (make-address prefix (ash index 3))))))

(defmethod cell-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod cell-unsigned-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-unsigned-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod quad-byte-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf quad-byte-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod double-byte-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf double-byte-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod byte-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf byte-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod space-decode-address ((sp state-space) address &optional size-hint)
  (declare (ignore address size-hint))
  (forth-exception :invalid-memory))


;;; Systems reserve "Page 0" of the address space to protect against references through null pointers.

(defclass null-space (mspace)
  ((size :accessor null-space-size :initarg :size :initform #+Darwin 16384 #-Darwin 4096))
  )

(defmethod print-object ((sp null-space) stream)
  (with-slots (prefix size) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X" prefix))))

(defmethod space-reset ((sp null-space))
  nil)

(defmethod save-space-state ((sp null-space))
  nil)

(defmethod space-allocate ((sp null-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :null-pointer-reference))

(defmethod space-deallocate ((sp null-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :null-pointer-reference))

(defmethod space-unused ((sp null-space))
  0)

(defmethod space-align ((sp null-space) &optional (boundary +cell-size+))
  (declare (ignore boundary))
  (forth-exception :null-pointer-reference))

(declaim (inline null-space-error))
(defun null-space-error (address why)
  (if (zerop address)
      (forth-exception :null-pointer-reference)
      (forth-exception :invalid-memory "Attempt to ~A #x~16,'0X" why address)))
      
(defmethod cell-at ((sp null-space) address)
  (null-space-error address "read from"))

(defmethod (setf cell-at) (value (sp null-space) address)
  (declare (ignore value))
  (null-space-error address "write to"))

(defmethod cell-unsigned-at ((sp null-space) address)
  (null-space-error address "read from"))

(defmethod (setf cell-unsigned-at) (value (sp null-space) address)
  (declare (ignore value))
  (null-space-error address "write to"))

(defmethod quad-byte-at ((sp null-space) address)
  (null-space-error address "read from"))

(defmethod (setf quad-byte-at) (value (sp null-space) address)
  (declare (ignore value))
  (null-space-error address "write to"))

(defmethod double-byte-at ((sp null-space) address)
  (null-space-error address "read from"))

(defmethod (setf double-byte-at) (value (sp null-space) address)
  (declare (ignore value))
  (null-space-error address "write to"))

(defmethod byte-at ((sp null-space) address)
  (null-space-error address "read from"))

(defmethod (setf byte-at) (value (sp null-space) address)
  (declare (ignore value))
  (null-space-error address "write to"))

(defmethod space-decode-address ((sp null-space) address &optional size-hint)
  (declare (ignore address size-hint))
  (forth-exception :null-pointer-reference))

(defmethod space-native-address ((sp null-space) foreign-address)
  (with-slots (prefix size) sp
    (when (<= 0 foreign-address (1- size))
      (make-address prefix foreign-address))))

(defmethod space-foreign-address ((sp null-space) native-address)
  (if (zerop native-address)
      0
      (null-space-error native-address "interpret as a pointer")))

(defmethod space-fill ((sp null-space) address count byte)
  (declare (ignore count byte))
  (null-space-error address "write to"))

(defmethod space-copy ((ssp null-space) source-address (dsp null-space) destination-address count)
  (declare (ignore destination-address count))
  (null-space-error source-address "read from"))

(defmethod space-copy ((ssp null-space) source-address (dsp mspace) destination-address count)
  (declare (ignore destination-address count))
  (null-space-error source-address "read from"))

(defmethod space-copy ((ssp mspace) source-address (dsp null-space) destination-address count)
  (declare (ignore source-address count))
  (null-space-error destination-address "write to"))


;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +number-of-spaces+ 256))

(defconstant +data-space-size+ (expt 2 24))
(defconstant +pad-space-size+ 1024)
(defconstant +transient-space-size+ 1024)
(defconstant +pictured-buffer-size+ 256)

(defconstant +name>string-space-size+ 256)
;; Forth 2012 states that there must be at least two transient buffers available for S" and S\"
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +number-of-string-spaces+ 16))

(deftype all-spaces-array () `(simple-array (or mspace null) (,+number-of-spaces+)))
(deftype string-spaces-array () `(simple-array (or mspace null) (,+number-of-string-spaces+)))

(defstruct (memory (:constructor %make-memory)
                   (:print-function %print-memory))
  (all-spaces (make-array 256 :initial-element nil) :type all-spaces-array)
  (data-space (make-instance 'data-space :size +data-space-size+) :type mspace)
  (pad (make-instance 'data-space :size +pad-space-size+) :type mspace)
  (word-space (make-instance 'transient-data-space :size +transient-space-size+) :type mspace)
  (pictured-buffer (make-instance 'pictured-buffer :size +pictured-buffer-size+) :type mspace)
  (name>string-space (make-instance 'transient-data-space :size +name>string-space-size+) :type mspace)
  (string-spaces (make-array +number-of-string-spaces+ :initial-element nil) :type string-spaces-array)
  (current-string-space-index 0 :type fixnum))

(defun make-memory ()
  (let ((memory (%make-memory)))
    (setf (aref (memory-all-spaces memory) 0) (make-instance 'null-space :prefix 0))
    (add-space memory (memory-data-space memory))
    (add-space memory (memory-pad memory))
    (add-space memory (memory-word-space memory))
    (add-space memory (memory-pictured-buffer memory))
    (add-space memory (memory-name>string-space memory))
    (dotimes (i +number-of-string-spaces+)
      (let ((space (make-instance 'transient-data-space :size +transient-space-size+)))
        (setf (aref (memory-string-spaces memory) i) space)
        (add-space memory space)))
    memory))

(defun %print-memory (memory stream depth)
  (declare (ignore depth))
  (print-unreadable-object (memory stream :type t :identity t)
    (format stream "~D space~:P" (count nil (memory-all-spaces memory) :test-not 'eq))))

(defun add-space (memory space)
  (let ((prefix (position nil (memory-all-spaces memory))))
    (if prefix
        (setf (aref (memory-all-spaces memory) prefix) space
              (space-prefix space) prefix)
        (forth-exception :data-space-overflow))))

(defun add-state-space (memory parent)
  (let ((space (make-instance 'state-space :parent parent)))
    (add-space memory space)))

(declaim (inline address-space))
(defun address-space (memory address)
  (declare (type memory memory) (fixnum address)
           (optimize (speed 3) (safety 0)))
  (let ((prefix (address-prefix address)))
    (or (aref (memory-all-spaces memory) prefix)
        (forth-exception :invalid-memory))))

(defmacro define-memory-fun (name arglist &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@arglist)
       (declare (type memory memory) (optimize (speed 3) (safety 0)))
       ,@body)))

(defmacro do-all-spaces (memory space &body body)
  `(dotimes (i +number-of-spaces+)
     (let ((,space (aref (memory-all-spaces ,memory) i)))
      (when ,space
        ,@body))))
  
(define-memory-fun reset-memory (memory)
  (do-all-spaces memory space
    (space-reset space)))

(define-memory-fun save-memory-state (memory)
  (do-all-spaces memory space
    (save-space-state space)))

(define-memory-fun reset-pictured-buffer (memory)
  (space-reset (memory-pictured-buffer memory)))

(define-memory-fun data-space-base-address (memory)
  (make-address (space-prefix (memory-data-space memory)) 0))

(define-memory-fun data-space-high-water-mark (memory)
  (let ((data-space (memory-data-space memory)))
    (make-address (space-prefix data-space) (space-high-water-mark data-space))))

(define-memory-fun data-space-unused (memory)
  (space-unused (memory-data-space memory)))

(define-memory-fun pad-base-address (memory)
  (make-address (space-prefix (memory-pad memory)) 0))

(define-memory-fun transient-space-base-address (memory space)
  (declare (ignore memory))
  (make-address (space-prefix space) 0))

(define-memory-fun ensure-transient-space-holds (memory space n-bytes)
  (declare (ignore memory))
  (space-reset space)
  (space-unseal space)
  (space-allocate space n-bytes))

(define-memory-fun seal-transient-space (memory space-or-address)
  (etypecase space-or-address
    (mspace 
     (space-seal space-or-address))
    (integer
     (space-seal (address-space memory space-or-address)))))

(define-memory-fun reserve-string-space (memory)
  (prog1
      (aref (memory-string-spaces memory) (memory-current-string-space-index memory))
    (setf (memory-current-string-space-index memory) (the fixnum (1+ (memory-current-string-space-index memory))))
    (when (= (memory-current-string-space-index memory) +number-of-string-spaces+)
      (setf (memory-current-string-space-index memory) 0))))

(define-memory-fun state-slot-address (memory slot)
  (do-all-spaces memory space
    (let ((address (space-state-slot-address space slot)))
      (when address
        (return-from state-slot-address address))))
  (forth-exception :unknown-slot))

(define-memory-fun memory-usage (memory)
  (let ((usage 0))
    (incf usage (space-high-water-mark (memory-data-space memory)))
    (incf usage (space-high-water-mark (memory-word-space memory)))
    (incf usage (space-high-water-mark (memory-name>string-space memory)))
    (dotimes (i +number-of-string-spaces+)
      (incf usage (space-high-water-mark (aref (memory-string-spaces memory) i))))
    usage))

;;;

(defmethod save-to-template ((memory memory))
  (let ((template (make-array +number-of-spaces+ :initial-element nil)))
    (dotimes (i +number-of-spaces+)
      (let ((space (aref (memory-all-spaces memory) i)))
        (when space
          (setf (aref template i) (space-save-to-template space)))))
    template))

(defmethod load-from-template (memory template fs)
  (declare (ignore fs))
  (dotimes (i +number-of-spaces+)
    (let ((space (aref (memory-all-spaces memory) i))
          (space-template (aref template i)))
      (if (and space space-template)
          (space-load-from-template space space-template)
          (assert (and (null space) (null space-template)) () "Memory template mismatch")))))

;;;

(define-memory-fun  allocate-memory (memory n-bytes)
  (space-allocate (memory-data-space memory) n-bytes))

(define-memory-fun deallocate-memory ( memory n-bytes)
  (space-deallocate (memory-data-space memory) n-bytes))
 
(define-memory-fun align-memory (memory &optional (boundary +cell-size+))
  (space-align (memory-data-space memory) boundary))

;;;

(define-memory-fun memory-cell (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (cell-at space address)))

(define-memory-fun (setf memory-cell) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (setf (cell-at space address) value)))

(define-memory-fun memory-cell-unsigned (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (cell-unsigned-at space address)))

(define-memory-fun (setf memory-cell-unsigned) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (setf (cell-unsigned-at space address) value)))

(define-memory-fun memory-double-cell (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address))
         (low (cell-unsigned-at space (+ address +cell-size+)))
         (high (cell-unsigned-at space address)))
    (double-cell-signed low high)))
      
(define-memory-fun (setf memory-double-cell) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (multiple-value-bind (low high)
        (double-components value)
      (setf (cell-unsigned-at space (+ address +cell-size+)) low)
      (setf (cell-unsigned-at space address) high))
    value))
  
(define-memory-fun memory-double-cell-unsigned (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address))
         (low (cell-unsigned-at space (+ address +cell-size+)))
         (high (cell-unsigned-at space address)))
    (double-cell-unsigned low high)))
      
(define-memory-fun (setf memory-double-cell-unsigned) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (multiple-value-bind (low high)
        (double-components value)
      (setf (cell-unsigned-at space (+ address +cell-size+)) low)
      (setf (cell-unsigned-at space address) high))
    value))

(define-memory-fun memory-quad-byte (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (quad-byte-at space address)))

(define-memory-fun (setf memory-quad-byte) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (setf (quad-byte-at space address) value)))

(define-memory-fun memory-double-byte (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (double-byte-at space address)))

(define-memory-fun (setf memory-double-byte) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (setf (double-byte-at space address) value)))

(define-memory-fun  memory-byte (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (byte-at space address)))

(define-memory-fun (setf memory-byte) (value memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (setf (byte-at space address) value)))

;;; In 2016, the Forth standardization committee adopted a proposal that characters occupy
;;; one address unit (i.e., byte) in memory

(declaim (inline memory-char))
(defun memory-char (memory address)
  (memory-byte memory address))

(declaim (inline (setf memory-char)))
(defun (setf memory-char) (value memory address)
  (setf (memory-byte memory address) value))

;;; 

(declaim (inline memory-single-float))
(defun memory-single-float (memory address)
  (encode-single-float (memory-quad-byte memory address)))
    
(declaim (inline (setf memory-single-float)))
(defun (setf memory-single-float) (value memory address)
  (setf (memory-quad-byte memory address) (decode-single-float value))
  value)

(declaim (inline memory-double-float))
(defun memory-double-float (memory address)
  (encode-double-float (memory-cell-unsigned memory address)))

(declaim (inline (setf memory-double-float)))
(defun (setf memory-double-float) (value memory address)
  (setf (memory-cell-unsigned memory address) (decode-double-float value))
  value)

;;; CL-Forth uses double precision floating point as its internal representation of float values

(declaim (inline memory-native-float))
(defun memory-native-float (memory address)
  (memory-double-float memory address))

(declaim (inline (setf memory-native-float)))
(defun (setf memory-native-float) (value memory address)
  (setf (memory-double-float memory address) value))

;;;

(define-memory-fun memory-fill (memory address count byte)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (space-fill space address count byte)))

(define-memory-fun memory-copy (memory source destination count)
  (let* ((source-space (address-space memory source))
         (source-address (address-address source))
         (destination-space (address-space memory destination))
         (destination-address (address-address destination)))
    (space-copy source-space source-address destination-space destination-address count)))

;;;

(define-memory-fun memory-decode-address (memory address &optional size-hint)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (space-decode-address space address size-hint)))

(define-memory-fun native-address (memory foreign-pointer)
  (let ((foreign-address (pointer-address foreign-pointer)))
    (do-all-spaces memory space
      (let ((address (space-native-address space foreign-address)))
        (when address
          (return-from native-address address)))))
  (forth-exception :invalid-memory))

(define-memory-fun foreign-pointer (memory native-address)
  (let* ((space (address-space memory native-address))
         (address (address-address native-address)))
    (address-pointer (space-foreign-address space address))))

(define-memory-fun address-is-foreign? (memory address)
  (let* ((space (address-space memory address))
         (address (address-address address)))
    (space-address-is-foreign? space address)))

;;;

(defconstant +native-memory-operation-success+ 0)
(defconstant +native-memory-operation-failure+ -2)

(define-memory-fun allocate-native-memory (memory n-bytes)
  (multiple-value-bind (pointer ior)
      (allocate-foreign-memory n-bytes)
    (values (native-address memory pointer) ior)))

(define-memory-fun free-native-memory (memory address)
  (if (address-is-foreign? memory address)
      (free-foreign-memory (foreign-pointer memory address))
      +native-memory-operation-failure+))

(define-memory-fun resize-native-memory (memory address n-bytes)
  (if (address-is-foreign? memory address)
      (multiple-value-bind (pointer ior)
          (resize-foreign-memory (foreign-pointer memory address) n-bytes)
        (values (native-address memory pointer) ior))
      (values address +native-memory-operation-failure+)))
