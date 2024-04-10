(in-package #:forth)

(defconstant +cell-size+ 8)
(defconstant +byte-to-cell-shift+ -3)

(defconstant +byte-to-double-byte-shift+ -1)
(defconstant +byte-to-quad-byte-shift+ -2)

(defconstant +single-float-cell-size+ 4)
(defconstant +byte-to-single-float-cell-shift+ -2)

(defconstant +double-float-cell-size+ 8)
(defconstant +byte-to-double-float-cell-shift+ -3)

;;; CL-Forth uses double precision floating point as its internal representation of float values
(defconstant +native-float-cell-size+ +double-float-cell-size+)

(declaim (inline make-address))
(defun make-address (prefix address)
  (dpb prefix (byte 8 48) address))

(declaim (inline address-prefix))
(defun address-prefix (address)
  (ldb (byte 8 48) address))

(declaim (inline address-address))
(defun address-address (address)
  (ldb (byte 48 0) address))

;;;

(defconstant +data-space-size+ (expt 2 24))
(defconstant +pad-space-size+ 1024)
(defconstant +transient-space-size+ 1024)
(defconstant +pictured-buffer-size+ 256)
(defconstant +name>string-space-size+ 256)
;; Forth 2012 states that there must be at least two transient buffers available for S" and S\"
(defconstant +number-of-string-spaces+ 4)

(defclass memory ()
  ((all-spaces :initform (make-array 10 :fill-pointer 0 :adjustable t :initial-element nil))
   (data-space :initform (make-instance 'data-space :size +data-space-size+))
   (pad :initform (make-instance 'data-space :size +pad-space-size+))
   (word-space :reader word-space :initform (make-instance 'transient-data-space :size +transient-space-size+))
   (pictured-buffer :reader memory-pictured-buffer
                    :initform (make-instance 'pictured-buffer :size +pictured-buffer-size+))
   (name>string-space :reader name>string-space
                      :initform (make-instance 'transient-data-space :size +name>string-space-size+))
   (string-spaces :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (current-string-space-index :initform 0))
  )

(defmethod initialize-instance :after ((memory memory) &key &allow-other-keys)
  (with-slots (all-spaces data-space pad word-space pictured-buffer name>string-space string-spaces) memory
    (flet ((setup (space)
             (setf (space-prefix space) (vector-push-extend space all-spaces))))
      (setup (make-instance 'data-space :size +cell-size+))
      (setup data-space)
      (setup pad)
      (setup word-space)
      (setup pictured-buffer)
      (setup name>string-space)
      (dotimes (i +number-of-string-spaces+)
        (let ((space (make-instance 'transient-data-space :size +transient-space-size+)))
          (vector-push-extend space string-spaces)
          (setup space))))))

(defmethod print-object ((memory memory) stream)
  (with-slots (all-spaces) memory
    (print-unreadable-object (memory stream :type t :identity t)
      (format stream "~D space~:P" (length all-spaces)))))

(defmethod add-space ((memory memory) space)
  (with-slots (all-spaces) memory
    (setf (space-prefix space) (vector-push-extend space all-spaces))))

(defmethod add-state-space ((memory memory) parent)
  (with-slots (all-spaces) memory
    (let ((space (make-instance 'state-space :parent parent)))
      (setf (space-prefix space) (vector-push-extend space all-spaces)))))

(defmethod reset-memory ((memory memory))
  (with-slots (all-spaces) memory
    (dotimes (i (length all-spaces))
      (space-reset (aref all-spaces i)))))

(defmethod save-memory-state ((memory memory))
  (with-slots (all-spaces) memory
    (dotimes (i (length all-spaces))
      (save-space-state (aref all-spaces i)))))

(defmethod reset-pictured-buffer ((memory memory))
  (with-slots (pictured-buffer) memory
    (space-reset pictured-buffer)))

(defmethod data-space-base-address ((memory memory))
  (with-slots (data-space) memory
    (make-address (space-prefix data-space) 0)))

(defmethod data-space-high-water-mark ((memory memory))
  (with-slots (data-space) memory
    (make-address (space-prefix data-space) (space-high-water-mark data-space))))

(defmethod data-space-unused ((memory memory))
  (with-slots (data-space) memory
    (space-unused data-space)))

(defmethod pad-base-address ((memory memory))
  (with-slots (pad) memory
    (make-address (space-prefix pad) 0)))

(defmethod transient-space-base-address ((memory memory) space)
  (make-address (space-prefix space) 0))

(defmethod ensure-transient-space-holds ((memory memory) space n-bytes)
  (space-reset space)
  (space-unseal space)
  (space-allocate space n-bytes))

(defmethod seal-transient-space ((memory memory) space)
  (space-seal space))

(defmethod reserve-string-space ((memory memory))
  (with-slots (string-spaces current-string-space-index) memory
    (prog1
        (aref string-spaces current-string-space-index)
      (incf current-string-space-index)
      (when (= current-string-space-index +number-of-string-spaces+)
        (setf current-string-space-index 0)))))

(defmethod state-slot-address ((memory memory) slot)
  (with-slots (all-spaces) memory
    (or (loop for space across all-spaces
                thereis (and (typep space 'state-space) (state-slot-address space slot)))
        (forth-exception :unknown-slot))))

;;;

(defmethod allocate-memory ((memory memory) n-bytes)
  (with-slots (data-space) memory
    (space-allocate data-space n-bytes)))

(defmethod deallocate-memory ((memory memory) n-bytes)
  (with-slots (data-space) memory
    (space-deallocate data-space n-bytes)))
 
(defmethod align-memory ((memory memory) &optional (boundary +cell-size+))
  (with-slots (data-space) memory
    (space-align data-space boundary)))

(defmethod memory-cell ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (cell-at space address))))

(defmethod (setf memory-cell) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (cell-at space address) value))))

(defmethod memory-cell-unsigned ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (cell-unsigned-at space address))))

(defmethod (setf memory-cell-unsigned) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (cell-unsigned-at space address) value))))

(defmethod memory-double-cell ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address))
           (low (cell-unsigned-at space (+ address +cell-size+)))
           (high (cell-unsigned-at space address)))
      (double-cell-signed low high))))
      
(defmethod (setf memory-double-cell) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (multiple-value-bind (low high)
          (double-components value)
        (setf (cell-unsigned-at space (+ address +cell-size+)) low)
        (setf (cell-unsigned-at space address) high))
      value)))
  
(defmethod memory-double-cell-unsigned ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address))
           (low (cell-unsigned-at space (+ address +cell-size+)))
           (high (cell-unsigned-at space address)))
      (double-cell-unsigned low high))))
      
(defmethod (setf memory-double-cell-unsigned) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (multiple-value-bind (low high)
          (double-components value)
        (setf (cell-unsigned-at space (+ address +cell-size+)) low)
        (setf (cell-unsigned-at space address) high))
      value)))

(defmethod memory-quad-byte ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (quad-byte-at space address))))

(defmethod (setf memory-quad-byte) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (quad-byte-at space address) value))))

(defmethod memory-double-byte ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (double-byte-at space address))))

(defmethod (setf memory-double-byte) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (double-byte-at space address) value))))

(defmethod memory-byte ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (byte-at space address))))

(defmethod (setf memory-byte) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (byte-at space address) value))))

;;;--- NOTE: If we change the size of a character to be other than 1 byte,
;;;---  these next two methods will need some rework

(defmethod memory-char ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (byte-at space address))))

(defmethod (setf memory-char) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (byte-at space address) value))))

;;; 

(defmethod memory-single-float ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (single-float-at space address))))
    
(defmethod (setf memory-single-float) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (single-float-at space address) value))))

(defmethod memory-double-float ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (double-float-at space address))))
    
(defmethod (setf memory-double-float) (value (memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (setf (double-float-at space address) value))))

;;; CL-Forth uses double precision floating point as its internal representation of float values

(declaim (inline memory-native-float))
(defun memory-native-float (memory address)
  (memory-double-float memory address))

(declaim (inline (setf memory-native-float)))
(defun (setf memory-native-float) (value memory address)
  (setf (memory-double-float memory address) value))

;;;

(defmethod memory-fill ((memory memory) address count byte)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (space-fill space address count byte))))

(defmethod memory-copy ((memory memory) source destination count)
  (with-slots (all-spaces) memory
    (let* ((source-prefix (address-prefix source))
           (source-space (aref all-spaces source-prefix))
           (source-address (address-address source))
           (destination-prefix (address-prefix destination))
           (destination-space (aref all-spaces destination-prefix))
           (destination-address (address-address destination)))
      (space-copy source-space source-address destination-space destination-address count))))

;;;

(defmethod memory-decode-address ((memory memory) address)
  (with-slots (all-spaces) memory
    (let* ((prefix (address-prefix address))
           (space (aref all-spaces prefix))
           (address (address-address address)))
      (space-decode-address space address))))


;;;

(defclass space ()
  ((prefix :accessor space-prefix :initarg :prefix :initform nil)
   (high-water-mark :accessor space-high-water-mark :initform 0))
  )

(defmethod print-object ((sp space) stream)
  (with-slots (prefix high-water-mark) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X, hwm=~D" prefix high-water-mark))))

(defgeneric space-reset (space))
(defgeneric save-space-state (space))

(defgeneric space-seal (space)
  (:method ((sp space)) nil))
(defgeneric space-unseal (space)
  (:method ((sp space)) nil))

(defgeneric space-allocate (space n-bytes))
(defgeneric space-deallocate (space n-bytes))
(defgeneric space-unused (space)
  (:method ((sp space)) 0))
(defgeneric space-align (space &optional boundary))

(defgeneric cell-at (space address))
(defgeneric (setf cell-at) (value space address))

(defgeneric cell-unsigned-at (space address))
(defgeneric (setf cell-unsigned-at) (value space address))

(defgeneric quad-byte-at (space address))
(defgeneric (setf quad-byte-at) (value space address))

(defgeneric double-byte-at (space address))
(defgeneric (setf double-byte-at) (value space address))

(defgeneric byte-at (space address))
(defgeneric (setf byte-at) (value space address))

(defgeneric single-float-at (space address))
(defgeneric (setf single-float-at) (value space address))

(defgeneric double-float-at (space address))
(defgeneric (setf double-float-at) (value space address))

(defgeneric space-fill (space address count byte))
(defgeneric space-copy (source-space source-address destination-space destination-address count))

(defgeneric space-decode-address (space address))

;;;

(defclass data-space (space)
  ((data :accessor data-space-data)
   (size :accessor data-space-size :initarg :size :initform 0)
   (extension :initform 0)
   (saved-high-water-mark :initform 0))
  )

(defmethod initialize-instance :after ((sp data-space) &key &allow-other-keys)
  (with-slots (data size extension) sp
    (assert (and (numberp size) (plusp size)) (size) "~S ~S must be a positive integer" 'data-space :size)
    (setf data (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
    (setf extension (floor size 10))))

(defmethod print-object ((sp data-space) stream)
  (with-slots (prefix data size high-water-mark) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X, size=~D, hwm=~D" prefix size high-water-mark))))

(defmethod space-reset ((sp data-space))
  (with-slots (high-water-mark saved-high-water-mark) sp
    (setf high-water-mark saved-high-water-mark))
  nil)

(defmethod save-space-state ((sp data-space))
  (with-slots (high-water-mark saved-high-water-mark) sp
    (setf saved-high-water-mark high-water-mark))
  nil)

(defmethod space-allocate ((sp data-space) n-bytes)
  (with-slots (prefix size high-water-mark data extension) sp
    (let ((address (make-address prefix high-water-mark)))
      (unless (<= (+ high-water-mark n-bytes) size)
        (forth-exception :data-space-overflow)
        #+ignore
        (let* ((new-size (+ high-water-mark n-bytes extension))
               ;; Grow the space by an additional 10% beyond what was requested
               (new (make-array new-size :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace new data :end1 size)
          (setf data new)))
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
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (cell-signed (aref data (ash address +byte-to-cell-shift+))))))

(defmethod (setf cell-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (setf (aref data (ash address +byte-to-cell-shift+)) (cell-unsigned value)))))

(defmethod cell-unsigned-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (aref data (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-unsigned-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (setf (aref data (ash address +byte-to-cell-shift+)) value))))

(defmethod quad-byte-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 32)) data))
      (quad-byte-signed (aref data (ash address +byte-to-quad-byte-shift+))))))

(defmethod (setf quad-byte-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 32)) data))
      (setf (aref data (ash address +byte-to-quad-byte-shift+)) (quad-byte-unsigned value)))))

(defmethod double-byte-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 16)) data))
      (double-byte-signed (aref data (ash address +byte-to-double-byte-shift+))))))

(defmethod (setf double-byte-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 16)) data))
      (setf (aref data (ash address +byte-to-double-byte-shift+)) (double-byte-unsigned value)))))

(defmethod byte-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (aref data address)))

(defmethod (setf byte-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (setf (aref data address) (ldb (byte 8 0) value))))

(defmethod single-float-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 32)) data))
      (encode-single-float (aref data (ash address +byte-to-single-float-cell-shift+))))))

(defmethod (setf single-float-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 32)) data))
      (setf (aref data (ash address +byte-to-single-float-cell-shift+)) (decode-single-float value))
      value)))

(defmethod double-float-at ((sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (encode-double-float (aref data (ash address +byte-to-double-float-cell-shift+))))))

(defmethod (setf double-float-at) (value (sp data-space) address)
  (with-slots (data size) sp
    (unless (<= address size)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (setf (aref data (ash address +byte-to-double-float-cell-shift+)) (decode-double-float value))
      value)))

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

(defmethod space-decode-address ((sp data-space) address)
  (with-slots (data) sp
    (values data address)))


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

(defmethod (setf single-float-at) :before (value (sp transient-data-space) address)
  (declare (ignore value address))
  (with-slots (active?) sp
    (unless active?
      (forth-exception :write-to-read-only-memory))))

(defmethod (setf double-float-at) :before (value (sp transient-data-space) address)
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
  (with-slots (data size used) pb
    (when (>= (+ used count) size)
      (forth-exception :pictured-output-overflow))
    (multiple-value-bind (source-data offset)
        (memory-decode-address memory address)
      (replace data source-data :start1 (- size used count) :end1 (- size used)
                                :start2 offset :end2 (+ offset count))
      (incf used count))))

(defmethod finish-pictured-buffer ((pb pictured-buffer))
  (with-slots (active? prefix size used) pb
    (setf active? nil)
    (let ((idx (- size used)))
      (values (make-address prefix idx) used))))


;;;

(defclass state-space (space)
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

(defmethod state-slot-address ((sp state-space) slot)
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

(defmethod single-float-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf single-float-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod double-float-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf double-float-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (unless (< (ash address +byte-to-cell-shift+) (length slots))
      (forth-exception :invalid-memory))
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod space-fill ((sp state-space) address count byte)
  (declare (ignore address count byte))
  (forth-exception :invalid-memory))

(defmethod space-copy ((ssp state-space) source-address (dsp space) destination-address count)
  (declare (ignore source-address destination-address count))
  (forth-exception :invalid-memory))

(defmethod space-copy ((ssp space) source-address (dsp state-space) destination-address count)
  (declare (ignore source-address destination-address count))
  (forth-exception :invalid-memory))

(defmethod space-decode-address ((sp state-space) address)
  (declare (ignore address))
  (forth-exception :invalid-memory))
