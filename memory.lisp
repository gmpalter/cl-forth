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

(defconstant +data-space-size+ (expt 2 20))
(defconstant +pad-and-temp-space-size+ 1024)
(defconstant +pictured-buffer-size+ 256)
 
(defclass memory ()
  ((all-spaces :initform (make-array 10 :fill-pointer 0 :initial-element nil))
   (data-space :initform (make-instance 'data-space :initial-size +data-space-size+))
   (pad :initform (make-instance 'data-space :initial-size +pad-and-temp-space-size+))
   (temp-space :initform (make-instance 'data-space :initial-size +pad-and-temp-space-size+))
   (pictured-buffer :reader memory-pictured-buffer
                    :initform (make-instance 'pictured-buffer :initial-size +pictured-buffer-size+)))
  )

(defmethod initialize-instance :after ((memory memory) &key &allow-other-keys)
  (with-slots (all-spaces data-space pad temp-space pictured-buffer) memory
    (flet ((setup (space)
             (setf (space-prefix space) (vector-push-extend space all-spaces))))
      (setup (make-instance 'data-space :initial-size 0))
      (setup data-space)
      (setup pad)
      ;; FILL, MOVE, ERASE, and BLANK all check the high water mark to decide if the operation is valid
      (setf (space-high-water-mark pad) +pad-and-temp-space-size+)
      (setup temp-space)
      (setup pictured-buffer))))

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

(defmethod pad-base-address ((memory memory))
  (with-slots (pad) memory
    (make-address (space-prefix pad) 0)))

(defmethod temp-space-base-address ((memory memory))
  (with-slots (temp-space) memory
    (make-address (space-prefix temp-space) 0)))

(defmethod ensure-temp-space-holds ((memory memory) n-bytes)
  (with-slots (temp-space) memory
    (space-reset temp-space)
    (space-allocate temp-space n-bytes)))

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
      (format stream "prefix=~2,'0X, used=~D" prefix high-water-mark))))

(defgeneric space-reset (space))
(defgeneric save-space-state (space))

(defgeneric space-allocate (space n-bytes))
(defgeneric space-deallocate (space n-bytes))
(defgeneric space-align (space))

(defgeneric cell-at (space address))
(defgeneric (setf cell-at) (value space address))

(defgeneric cell-unsigned-at (space address))
(defgeneric (setf cell-unsigned-at) (value space address))

(defgeneric byte-at (space address))
(defgeneric (setf byte-at) (value space address))

(defgeneric space-fill (space address count byte))
(defgeneric space-copy (source-space source-address destination-space destination-address count))

(defgeneric space-decode-address (space address))

;;;

(defclass data-space (space)
  ((data :accessor data-space-data)
   (extension :initform 0)
   (saved-high-water-mark :initform 0))
  )

(defmethod initialize-instance :after ((sp data-space) &key initial-size &allow-other-keys)
  (with-slots (data extension) sp
    (setf data (make-array initial-size :element-type '(unsigned-byte 8) :initial-element 0))
    (setf extension (floor initial-size 10))))

(defmethod print-object ((sp data-space) stream)
  (with-slots (prefix data high-water-mark) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X, size=~D, used=~D" prefix (length data) high-water-mark))))

(defmethod space-reset ((sp data-space))
  (with-slots (high-water-mark saved-high-water-mark) sp
    (setf high-water-mark saved-high-water-mark))
  nil)

(defmethod save-space-state ((sp data-space))
  (with-slots (high-water-mark saved-high-water-mark) sp
    (setf saved-high-water-mark high-water-mark))
  nil)

(defmethod space-allocate ((sp data-space) n-bytes)
  (with-slots (prefix high-water-mark data extension) sp
    (let ((address (make-address prefix high-water-mark)))
      (unless (<= (+ high-water-mark n-bytes) (length data))
        (let* ((new-size (+ high-water-mark n-bytes extension))
               ;; Grow the space by an additional 10% beyond what was requested
               (new (make-array new-size :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace new data :end1 (length data))
          (setf data new)))
      (incf high-water-mark n-bytes)
      address)))

(defmethod space-deallocate ((sp data-space) n-bytes)
  (with-slots (high-water-mark) sp
    (decf high-water-mark (min high-water-mark n-bytes))))

(defmethod space-align ((sp data-space))
  (with-slots (high-water-mark) sp
    (unless (zerop (mod high-water-mark +cell-size+))
      (incf high-water-mark (- +cell-size+ (mod high-water-mark +cell-size+))))))

(defmethod cell-at ((sp data-space) address)
  (with-slots (data high-water-mark) sp
    (unless (<= address high-water-mark)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (cell-signed (aref data (ash address +byte-to-cell-shift+))))))

(defmethod (setf cell-at) (value (sp data-space) address)
  (with-slots (data high-water-mark) sp
    (unless (<= address high-water-mark)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (setf (aref data (ash address +byte-to-cell-shift+)) (cell-unsigned value)))))

(defmethod cell-unsigned-at ((sp data-space) address)
  (with-slots (data high-water-mark) sp
    (unless (<= address high-water-mark)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (aref data (ash address +byte-to-cell-shift+)))))

(defmethod (setf cell-unsigned-at) (value (sp data-space) address)
  (with-slots (data high-water-mark) sp
    (unless (<= address high-water-mark)
      (forth-exception :invalid-memory))
    (locally (declare (optimize (speed 3) (safety 0))
                      (type (simple-array (unsigned-byte 64)) data))
      (setf (aref data (ash address +byte-to-cell-shift+)) value))))

(defmethod byte-at ((sp data-space) address)
  (with-slots (data high-water-mark) sp
    (unless (<= address high-water-mark)
      (forth-exception :invalid-memory))
    (aref data address)))

(defmethod (setf byte-at) (value (sp data-space) address)
  (with-slots (data high-water-mark) sp
    (unless (<= address high-water-mark)
      (forth-exception :invalid-memory))
    (setf (aref data address) value)))

(defmethod space-fill ((sp data-space) address count byte)
  (with-slots (data high-water-mark) sp
    (unless (<= (+ address count) high-water-mark)
      (forth-exception :invalid-memory))
    (fill data byte :start address :end (+ address count))))

(defmethod space-copy ((ssp data-space) source-address (dsp data-space) destination-address count)
  (with-slots ((source-data data) (source-high-water-mark high-water-mark)) ssp
    (with-slots ((destination-data data) (destination-high-water-mark high-water-mark)) dsp
      (unless (<= (+ source-address count) source-high-water-mark)
        (forth-exception :invalid-memory))
      (unless (<= (+ destination-address count) destination-high-water-mark)
        (forth-exception :invalid-memory))
      (replace destination-data source-data :start1 destination-address :end1 (+ destination-address count)
                                            :start2 source-address :end2 (+ source-address count)))))

(defmethod space-decode-address ((sp data-space) address)
  (with-slots (data) sp
    (values data address)))


;;;

(defclass pictured-buffer (data-space)
  ((active? :accessor pictured-buffer-active? :initform nil)
   (used :initform 0))
  )

(defmethod initialize-instance :after ((pb pictured-buffer) &key &allow-other-keys)
  (with-slots (data high-water-mark) pb
    ;; All memory operations check the high water mark to decide if the operation is valid
    ;; As we fill the buffer from the end towards the front, we have to claim its all "in use"
    (setf high-water-mark (length data))))

(defmethod print-object ((pb pictured-buffer) stream)
  (with-slots (prefix high-water-mark used active?) pb
    (print-unreadable-object (pb stream :type t :identity t)
      (format stream "prefix=~2,'0X, size=~D, used=~D~@[, active~]" prefix high-water-mark used active?))))

(defmethod space-reset ((pb pictured-buffer))
  (with-slots (data high-water-mark used) pb
      (setf high-water-mark (length data)
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
  (with-slots (high-water-mark used) pb
    (when (= used high-water-mark)
      (forth-exception :pictured-output-overflow))
    (prog1
        (setf (byte-at pb (- high-water-mark used 1)) forth-char)
      (incf used))))

(defmethod finish-pictured-buffer ((pb pictured-buffer))
  (with-slots (active? prefix high-water-mark used) pb
    (setf active? nil)
    (let ((idx (- high-water-mark used)))
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

(defmethod space-fill ((sp state-space) address count byte)
  (declare (ignore address count byte))
  (error "Can't space-fill ~S" sp))

(defmethod space-copy ((ssp state-space) source-address (dsp space) destination-address count)
  (declare (ignore source-address destination-address count))
  (error "Can't copy from ~S" ssp))

(defmethod space-copy ((ssp space) source-address (dsp state-space) destination-address count)
  (declare (ignore source-address destination-address count))
  (error "Can't copy to ~S" ssp))

(defmethod space-decode-address ((sp state-space) address)
  (declare (ignore address))
  (error "Can't decode addresses in ~S" sp))
