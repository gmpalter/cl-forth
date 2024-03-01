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
   (next-source-id :initform 1)
   (source-id-map :initform (make-hash-table))
   (input-stack :initform nil)
   (source-stack :initform (make-instance 'stack :initial-size 16
                                                 :overflow-key :source-stack-overflow :underflow-key :source-stack-underflow))
   (source-as-space :accessor files-source-as-space :initform (make-instance 'source-data-space)))
  )

(defstruct saved-source
  (id 0)
  (>in 0)
  (buffer ""))

(defmethod reset-input ((f files))
  (with-slots (source-id >in buffer source-stack) f
    (setf source-id 0
          >in 0
          buffer "")
    (stack-reset source-stack)))

(defmethod terminal-input-p ((f files))
  (with-slots (source-id) f
    (zerop source-id)))

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
                   (setf word (adjust-array word word-end :fill-pointer word-end))
                   (replace word buffer :start1 word-start :end1 word-end :start2 >in :end2 end)))
            do (setf >in (1+ end))
            if (< >in buffer-len)
              do (return-from word word)
            else
              do (when (if (plusp source-id)
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
            do (setf >in (1+ end))
            if (< >in buffer-len)
              do (return-from parse parsed)
            else
              do (when (if (plusp source-id)
                           (null (refill f))
                           t)
                   (return-from parse parsed)
                   (vector-push-extend #\Space parsed))))))

(defmethod refill ((f files))
  (with-slots (source-id >in buffer source-id-map source-as-space) f
    (flet ((fillup (stream)
             (setf (source-data-space-is-valid? source-as-space) nil)
             (let ((line (read-line stream nil :eof)))
               (unless (eq line :eof)
                 (setf buffer line
                       >in 0)
                 t))))
      (cond ((= source-id -1) nil)
            ((zerop source-id)
             (fillup *standard-input*))
            (t
             (fillup (gethash source-id source-id-map)))))))

(defmethod source-push ((f files) &key file-id evaluate)
  (assert (not (and file-id evaluate)) () "Pass ~S or ~S but not both to ~S" :file-id :evaluate 'source-push)
  (with-slots (source-id >in buffer source-stack source-as-space) f
    (let ((ss (make-saved-source :id source-id :>in >in :buffer buffer)))
      (stack-push source-stack ss)
      ;; SOURCE-ID for EVALUATE is always -1
      (setf source-id (or file-id -1)
            >in 0
            buffer (or evaluate ""))
      (setf (source-data-space-is-valid? source-as-space) nil))))

(defmethod source-pop ((f files))
  (with-slots (source-id >in buffer source-stack) f
    (when (plusp source-id)
      (close-file f source-id))
    (let ((ss (stack-pop source-stack)))
      (setf source-id (saved-source-id ss)
            >in (saved-source->in ss)
            buffer (saved-source-buffer ss)))))

(defmethod access-source-buffer ((f files))
  (with-slots (buffer source-as-space) f
    (unless (source-data-space-is-valid? source-as-space)
      (update-source-data-space source-as-space buffer))
    (values (make-address (space-prefix source-as-space) 0) (space-high-water-mark source-as-space))))
