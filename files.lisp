(in-package #:forth)

(defclass files ()
  ((source-id :initform 0)
   (>in)
   (buffer)
   (next-source-id :initform 1)
   (source-id-map :initform (make-hash-table))
   (input-stack :initform nil)
   (source-stack :initform (make-instance 'stack :initial-size 16
                                                 :overflow-key :source-stack-overflow :underflow-key :source-stack-underflow))
   )
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

(defmethod flush-input ((f files))
  (with-slots (>in buffer) f
    (setf >in (length buffer))))

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
  (with-slots (source-id >in buffer source-id-map) f
    (flet ((fillup (stream)
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
  (with-slots (source-id >in buffer source-stack) f
    (let ((ss (make-saved-source :id source-id :>in >in :buffer buffer)))
      (stack-push source-stack ss)
      ;; SOURCE-ID for EVALUATE is always -1
      (setf source-id (or file-id -1)
            >in 0
            buffer (or evaluate "")))))

(defmethod source-pop ((f files))
  (with-slots (source-id >in buffer source-stack) f
    (when (plusp source-id)
      (close-file f source-id))
    (let ((ss (stack-pop source-stack)))
      (setf source-id (saved-source-id ss)
            >in (saved-source->in ss)
            buffer (saved-source-buffer ss)))))
