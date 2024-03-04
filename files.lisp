(in-package #:forth)

(defclass source-data-space (data-space)
  ((is-valid? :accessor source-data-space-is-valid? :initform nil))
  (:default-initargs :initial-size (expt 2 20)))

(defmethod update-source-data-space ((sp source-data-space) buffer)
  (with-slots (data high-water-mark is-valid?) sp
    (native-into-forth-string buffer data 0)
    (setf high-water-mark (length buffer)
          is-valid? t)))

(defmethod space-reset ((sp source-data-space))
  nil)

(defmethod save-space-state ((sp source-data-space))
  nil)

(defmethod space-allocate ((sp source-data-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :write-to-read-only-memory))

(defmethod space-align ((sp source-data-space))
  nil)

(defmethod (setf cell-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod (setf cell-unsigned-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod (setf byte-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod space-fill ((sp source-data-space) address count byte)
  (declare (ignore address count byte))
  (forth-exception :write-to-read-only-memory))

(defmethod space-copy ((ssp space) source-address (dsp source-data-space) destination-address count)
  (declare (ignore source-address destination-address count))
  (forth-exception :write-to-read-only-memory))

;;;

(defclass files ()
  ((source-id :initform 0)
   (>in)
   (buffer)
   (source-address :initform nil)
   (verbose :accessor files-verbose :initform nil)
   (source-id-map :initform (make-hash-table))
   (last-source-id :initform 0)
   (input-stack :initform nil)
   (source-stack :initform (make-instance 'stack :name "Source" :initial-size 16
                                                 :overflow-key :source-stack-overflow :underflow-key :source-stack-underflow))
   (source-as-space :accessor files-source-as-space :initform (make-instance 'source-data-space)))
  )

(defstruct saved-source
  (id 0)
  (>in 0)
  (buffer "")
  (source-address nil))

(defmethod reset-input ((f files))
  (with-slots (source-id >in buffer source-address source-stack) f
    (setf source-id 0
          >in 0
          buffer ""
          source-address nil)
    (stack-reset source-stack)))

(defmethod terminal-input-p ((f files))
  (with-slots (source-id) f
    (zerop source-id)))

(defmethod file-input-p ((f files))
  (with-slots (source-id) f
    (plusp source-id)))

(defmethod input-available-p ((f files))
  (with-slots (>in buffer) f
    (< >in (length buffer))))

(defmethod flush-input-line ((f files))
  (with-slots (>in buffer) f
    (setf >in (length buffer))))

(defmethod flush-input-file ((f files))
  (with-slots (source-id) f
    (if (plusp source-id)
        (source-pop f)
        (flush-input-line f))))

(defmethod word ((f files) delimiter)
  (with-slots (source-id >in buffer) f
    (let ((word (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
          (whitespace-delimiter-p (and (char-equal delimiter #\Space) (plusp source-id))))
      (loop for buffer-len = (length buffer)
            for skip = (or (if whitespace-delimiter-p
                               (position-if-not #'whitespacep buffer :start >in)
                               (position delimiter buffer :start >in :test-not #'char-equal))
                           (length buffer))
            do (setf >in skip)
            if (< >in buffer-len)
              do (loop-finish)
            else
              do (when (if (plusp source-id)
                           (null (refill f))
                           t)
                   (return-from word nil)))
      (loop for buffer-len = (length buffer)
            for end = (or (if whitespace-delimiter-p
                              (position-if #'whitespacep buffer :start >in)
                              (position delimiter buffer :start >in :test #'char-equal))
                          (length buffer))
            do (when (< >in end)
                 (let* ((count (- end >in))
                        (word-start (fill-pointer word))
                        (word-end (+ word-start count)))
                   (adjust-array word word-end :fill-pointer word-end)
                   (replace word buffer :start1 word-start :end1 word-end :start2 >in :end2 end)))
            do (setf >in end)
            if (< >in buffer-len)
              do (incf >in)
                 (return-from word word)
            else
              do (if (if (and (plusp source-id) (not whitespace-delimiter-p))
                         (null (refill f))
                         t)
                   (return-from word word)
                   (vector-push-extend #\Space word))))))

(defmethod parse ((f files) delimiter)
  (with-slots (source-id >in buffer) f
    (let ((parsed (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
          (whitespace-delimiter-p (and (char-equal delimiter #\Space) (plusp source-id))))
      (loop for buffer-len = (length buffer)
            for end = (or (if whitespace-delimiter-p
                              (position-if #'whitespacep buffer :start >in)
                              (position delimiter buffer :start >in :test #'char-equal))
                          (length buffer))
            do (when (< >in end)
                 (let* ((count (- end >in))
                        (parsed-start (fill-pointer parsed))
                        (parsed-end (+ parsed-start count)))
                   (setf parsed (adjust-array parsed parsed-end :fill-pointer parsed-end))
                   (replace parsed buffer :start1 parsed-start :end1 parsed-end :start2 >in :end2 end)))
            do (setf >in end)
            if (< >in buffer-len)
              do (incf >in)
                 (return-from parse parsed)
            else
              do (if (if (and (plusp source-id) (not whitespace-delimiter-p))
                         (null (refill f))
                         t)
                   (return-from parse parsed)
                   (vector-push-extend #\Space parsed))))))

(defmethod refill ((f files))
  (with-slots (source-id >in buffer verbose source-id-map source-as-space) f
    (flet ((fillup (stream)
             (setf (source-data-space-is-valid? source-as-space) nil)
             (let ((line (read-line stream nil :eof)))
               (unless (eq line :eof)
                 (setf buffer line
                       >in 0)
                 (when (and verbose (plusp source-id))
                   (write-line buffer))
                 t))))
      (cond ((= source-id -1) nil)
            ((zerop source-id)
             (fillup *standard-input*))
            (t
             (fillup (gethash source-id source-id-map)))))))

(defmethod source-push ((f files) &key file-id evaluate ((:source-address source-address-override)))
  (assert (not (and file-id evaluate)) () "Pass ~S or ~S but not both to ~S" :file-id :evaluate 'source-push)
  (with-slots (source-id >in buffer source-address source-stack source-as-space) f
    (let ((ss (make-saved-source :id source-id :>in >in :buffer buffer :source-address source-address)))
      (stack-push source-stack ss)
      ;; SOURCE-ID for EVALUATE is always -1
      (setf source-id (or file-id -1)
            >in 0
            buffer (or evaluate "")
            ;; SOURCE should return the user's buffer address for EVALUATE
            source-address source-address-override)
      (setf (source-data-space-is-valid? source-as-space) nil))))

(defmethod source-pop ((f files))
  (with-slots (source-id >in buffer source-address source-stack) f
    (when (plusp source-id)
      (forth-close-file f source-id))
    (let ((ss (stack-pop source-stack)))
      (setf source-id (saved-source-id ss)
            >in (saved-source->in ss)
            buffer (saved-source-buffer ss)
            source-address (saved-source-source-address ss)))))

(defmethod access-source-buffer ((f files))
  (with-slots (buffer source-address source-as-space) f
    (unless (source-data-space-is-valid? source-as-space)
      (update-source-data-space source-as-space buffer))
    (values (or source-address (make-address (space-prefix source-as-space) 0)) (space-high-water-mark source-as-space))))


;;;

(defconstant +read-direction+  1)
(defconstant +write-direction+ 2)

(defconstant +binary-mode+ #x100)

(defconstant +file-operation-success+ 0)
;; Unfortunately, CCL doesn't include the actual OS status code in its FILE-ERROR condition
(defconstant +file-operation-failure+ -1)

(defun interpret-file-access-method (fam)
  (values (cond ((and (logtest fam +read-direction+) (logtest fam +write-direction+))
                 :io)
                ((logtest fam +read-direction+)
                 :input)
                ((logtest fam +write-direction+)
                 :output)
                (t
                 (forth-exception :file-i/o-exception "Unknown file access method")))
          (if (logtest fam +binary-mode+)
              '(unsigned-byte 8)
              'character)))

;;; Forth's definition of OPEN-FILE specifies that the file must exist. It also specifies that the file be positioned
;;; to the beginning of the file. (It doesn't say what should happen if opening for output.)
(defmethod forth-open-file ((f files) pathname fam)
  (with-slots (source-id-map last-source-id) f
    (multiple-value-bind (direction element-type)
        (interpret-file-access-method fam)
      (let ((stream (open pathname :direction direction :element-type element-type :if-exists :append :if-does-not-exist nil)))
        (if stream
            (let ((file-id (incf last-source-id)))
              (setf (gethash file-id source-id-map) stream)
              (file-position stream 0)
              (values file-id +file-operation-success+))
            (values 0 +file-operation-failure+))))))

;; Forth's definition of CREATE-FILE specifies that it replaces an existing file
(defmethod forth-create-file ((f files) pathname fam)
  (with-slots (source-id-map last-source-id) f
    (multiple-value-bind (direction element-type)
        (interpret-file-access-method fam)
      (let ((stream (open pathname :direction direction :element-type element-type :if-exists :supersede
                                   :if-does-not-exist :create)))
        (if stream
            (let ((file-id (incf last-source-id)))
              (setf (gethash file-id source-id-map) stream)
              (values file-id +file-operation-success+))
            (values 0 +file-operation-failure+))))))

(defmethod forth-close-file ((f files) file-id)
  (with-slots (source-id-map) f
    (let* ((stream (or (gethash file-id source-id-map) (forth-exception :file-i/o-exception "Invalid FILE-ID"))))
      (remhash file-id source-id-map)
      (handler-case
          (progn
            (close stream)
            +file-operation-success+)
        (file-error () +file-operation-failure+)))))
