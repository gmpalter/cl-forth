(in-package #:forth)

(defconstant +cell-size+ 8)
(defconstant +byte-to-cell-shift+ -3)

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

(defclass memory ()
  ((all-spaces :initform (make-array 10 :fill-pointer 0 :initial-element nil))
   (data-space :initform (make-instance 'data-space :initial-size (expt 2 20)))
   (pad :initform (make-instance 'data-space :initial-size 1024))
   (temp-space :initform (make-instance 'data-space :initial-size 1024)))
  )

(defmethod initialize-instance :after ((memory memory) &key &allow-other-keys)
  (with-slots (all-spaces data-space pad temp-space) memory
    (flet ((setup (space)
             (setf (space-prefix space) (vector-push-extend space all-spaces))))
      (setup (make-instance 'data-space :initial-size 0))
      (setup data-space)
      (setup pad)
      (setup temp-space))))

(defmethod add-state-space ((memory memory) parent)
  (with-slots (all-spaces) memory
    (let ((space (make-instance 'state-space :parent parent)))
      (setf (space-prefix space) (vector-push-extend space all-spaces)))))

(defmethod reset-memory ((memory memory))
  (with-slots (all-spaces) memory
    (dotimes (i (length all-spaces))
      (space-reset (aref all-spaces i)))))

(defmethod data-space-base-address ((memory memory))
  (with-slots (data-space) memory
    (make-address (space-prefix data-space) 0)))

(defmethod data-space-high-water-mark ((memory memory))
  (with-slots (data-space) memory
    (make-address (space-prefix data-space) (space-high-water-mark data-space))))

(defmethod pad-base-address ((memory memory))
  (with-slots (pad) memory
    (make-address (space-prefix pad) 0)))

(defmethod temp-space-base-address ((memory memory))
  (with-slots (temp-space) memory
    (make-address (space-prefix temp-space) 0)))

(defmethod state-slot-address ((memory memory) slot)
  (with-slots (all-spaces) memory
    (or (loop for space across all-spaces
                thereis (and (typep space 'state-space) (state-slot-address space slot)))
        (forth-error "Unknown slot" -101))))

;;;

(defmethod allocate-memory ((memory memory) n-bytes)
  (with-slots (data-space) memory
    (space-allocate data-space n-bytes)))

(defmethod align-memory ((memory memory))
  (with-slots (data-space) memory
    (space-align data-space)))

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

(defclass space ()
  ((prefix :accessor space-prefix :initarg :prefix :initform nil)
   (high-water-mark :accessor space-high-water-mark :initform 0))
  )

(defgeneric space-reset (space))

(defgeneric space-allocate (space n-bytes))
(defgeneric space-align (space))

(defgeneric cell-at (space address))
(defgeneric (setf cell-at) (value space address))

(defgeneric cell-unsigned-at (space address))
(defgeneric (setf cell-unsigned-at) (value space address))

(defgeneric byte-at (space address))
(defgeneric (setf byte-at) (value space address))

(defgeneric space-fill (space address count byte))
(defgeneric space-copy (source-space source-address destination-space destination-address count))


;;;

(defclass data-space (space)
  ((data :accessor data-space-data)
   (extension :initform 0))
  )

(defmethod initialize-instance :after ((sp data-space) &key initial-size &allow-other-keys)
  (with-slots (data extension) sp
    (setf data (make-array initial-size :element-type '(unsigned-byte 8) :initial-element 0))
    (setf extension (floor initial-size 10))))

(defmethod space-reset ((sp data-space))
  (setf (space-high-water-mark sp) 0)
  nil)

(defmethod space-allocate ((sp data-space) n-bytes)
  (with-slots (prefix high-water-mark data extension) sp
    (let ((address (make-address prefix high-water-mark)))
      (unless (<= (+ high-water-mark n-bytes) (length data))
        (let ((new (make-array (+ (length data) extension) :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace new data :end1 (length data))
          (setf data new)))
      (incf high-water-mark n-bytes)
      address)))

(defmethod space-align ((sp data-space))
  (with-slots (high-water-mark) sp
    (unless (zerop (mod high-water-mark +cell-size+))
      (incf high-water-mark (- +cell-size+ (mod high-water-mark +cell-size+))))))

(defmethod cell-at ((sp data-space) address)
  (with-slots (data) sp
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (signed-byte 64)) data))
      (aref data (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-at) (value (sp data-space) address)
  (with-slots (data) sp
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (signed-byte 64)) data))
      (setf (aref data (ash address +byte-to-cell-shift+)) value))))

(defmethod cell-unsigned-at ((sp data-space) address)
  (with-slots (data) sp
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (aref data (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-unsigned-at) (value (sp data-space) address)
  (with-slots (data) sp
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (setf (aref data (ash address +byte-to-cell-shift+)) value))))

(defmethod byte-at ((sp data-space) address)
  (with-slots (data) sp
    (aref data address)))

(defmethod (setf byte-at) (value (sp data-space) address)
  (with-slots (data) sp
    (setf (aref data address) value)))

(defmethod space-fill ((sp data-space) address count byte)
  (with-slots (data high-water-mark) sp
    (unless (<= (+ address count) high-water-mark)
      (forth-error "Invalid memory address" -9))
    (fill data byte :start address :end (+ address count))))

(defmethod space-copy ((ssp data-space) source-address (dsp data-space) destination-address count)
  (with-slots ((source-data data) (source-high-water-mark high-water-mark)) ssp
    (with-slots ((destination-data data) (destination-high-water-mark high-water-mark)) dsp
      (unless (<= (+ source-address count) source-high-water-mark)
        (forth-error "Invalid memory address" -9))
      (unless (<= (+ destination-address count) destination-high-water-mark)
        (forth-error "Invalid memory address" -9))
      (replace destination-data source-data :start1 destination-address :end1 (+ destination-address count)
                                            :start2 source-address :end2 (+ source-address count)))))


;;;

(defclass state-space (space)
  ((parent :initarg :parent :initform nil)
   (slots :initform nil))
  )

(defmethod initialize-instance :after ((sp state-space) &key &allow-other-keys)
  (with-slots (parent slots) sp
    (setf slots (map 'vector 'slot-definition-name (class-direct-slots (class-of parent))))))

(defmethod space-reset ((sp state-space))
  nil)

(defmethod space-allocate ((sp state-space) n-bytes)
  (declare (ignore n-bytes))
  (error "Can't allocate space in ~S" sp))

(defmethod space-align ((sp state-space))
  nil)

(defmethod state-slot-address ((sp state-space) slot)
  (with-slots (prefix slots) sp
    (let ((index (position slot slots)))
      (when index
        (make-address prefix (ash index 3))))))

(defmethod cell-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod cell-unsigned-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-unsigned-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))

(defmethod byte-at ((sp state-space) address)
  (with-slots (parent slots) sp
    (slot-value parent (aref slots (ash address +byte-to-cell-shift+)))))

(defmethod (setf byte-at) (value (sp state-space) address)
  (with-slots (parent slots) sp
    (setf (slot-value parent (aref slots (ash address +byte-to-cell-shift+))) value)))
