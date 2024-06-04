(in-package #:forth)

(defclass source-data-space (data-space)
  ((is-valid? :accessor source-data-space-is-valid? :initform nil)
   (buffer :accessor source-data-space-buffer :initform nil))
  (:default-initargs :size +data-space-size+))

(defmethod update-source-data-space ((sp source-data-space))
  (with-slots (data size high-water-mark is-valid? buffer) sp
    (when buffer
      (native-into-forth-string buffer data 0)
      (setf size (length buffer)
            high-water-mark (length buffer)
            is-valid? t))))

(defmethod space-reset ((sp source-data-space))
  nil)

(defmethod save-space-state ((sp source-data-space))
  nil)

(defmethod space-save-to-template ((sp source-data-space))
  ;; Source buffer is created on-demand by use of SOURCE
  nil)

(defmethod space-load-from-template ((sp source-data-space) template)
  (declare (ignore template))
  nil)

(defmethod space-allocate ((sp source-data-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :write-to-read-only-memory))

(defmethod space-align ((sp source-data-space) &optional (boundary +cell-size+))
  (declare (ignore boundary))
  nil)

(defmethod space-decode-address :before ((sp source-data-space) address &optional size-hint)
  (declare (ignore address size-hint))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod cell-at :before ((sp source-data-space) address)
  (declare (ignore address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod (setf cell-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod cell-unsigned-at :before ((sp source-data-space) address)
  (declare (ignore address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod (setf cell-unsigned-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod quad-byte-at :before ((sp source-data-space) address)
  (declare (ignore address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod (setf quad-byte-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod double-byte-at :before ((sp source-data-space) address)
  (declare (ignore address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod (setf double-byte-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod byte-at :before ((sp source-data-space) address)
  (declare (ignore address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod (setf byte-at) (value (sp source-data-space) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod space-native-address :before ((sp source-data-space) foreign-address)
  (declare (ignore foreign-address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod space-foreign-address :before ((sp source-data-space) native-address)
  (declare (ignore native-address))
  (with-slots (is-valid?) sp
    (unless is-valid?
      (update-source-data-space sp))))

(defmethod space-fill :before ((sp source-data-space) address count byte)
  (declare (ignore address count byte))
  (forth-exception :write-to-read-only-memory))

(defmethod space-copy :before ((ssp source-data-space) source-address (dsp mspace) destination-address count)
  (declare (ignore source-address destination-address count))
  (with-slots (is-valid?) ssp
    (unless is-valid?
      (update-source-data-space ssp))))

(defmethod space-copy :before ((ssp mspace) source-address (dsp source-data-space) destination-address count)
  (declare (ignore source-address destination-address count))
  (forth-exception :write-to-read-only-memory))


;;;

(defstruct (file (:constructor %make-file) (:copier nil))
  %stream
  binary?
  buffer)

(declaim (inline make-file))
(defun make-file (stream binary?)
  (%make-file :%stream stream :binary? binary? :buffer nil))

(defclass files ()
  ((source-id :reader source-id :initform 0)
   >in
   buffer
   (source-address :initform nil)
   (verbose :accessor files-verbose :initform nil)
   (source-id-map :initform (make-hash-table))
   (last-source-id :initform 0)
   (source-stack :initform (make-stack "Source" 16 :source-stack-overflow :source-stack-underflow))
   (source-as-space :reader files-source-as-space :initform (make-instance 'source-data-space))
   (saved-buffer-space :reader files-saved-buffer-space :initform (make-instance 'data-space :size +data-space-size+))
   (included-files :initform (make-hash-table :test #'equal)))
  )

(defparameter +no-file+ (make-file nil nil))

(declaim (inline file-file-stream))
(defun file-file-stream (fileid map)
  (file-%stream (gethash fileid map +no-file+)))

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

(defmethod current-input-state ((f files))
  (with-slots (buffer >in) f
    (values buffer >in)))

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

;;;

(defmethod word ((f files) delimiter &key multiline? forth-values?)
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
              do (when (if (and (plusp source-id) multiline?)
                           (null (refill f))
                           t)
                   (if forth-values?
                       (return-from word (values 0 0))
                       (return-from word nil))))
      (loop with starting->in = >in
            with address = (when forth-values?
                             (multiple-value-bind (address count)
                                 (access-source-buffer f)
                               (declare (ignore count))
                               (make-address (address-prefix address) (+ (address-address address)
                                                                         (* starting->in +char-size+)))))
            for buffer-len = (length buffer)
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
                 (if forth-values?
                     (return-from word (values address (* (- >in starting->in 1) +char-size+)))
                     (return-from word word))
            else
              do (if (if (and (plusp source-id) multiline?)
                         (null (refill f))
                         t)
                     (if forth-values?
                         (return-from word (values address (* (- >in starting->in) +char-size+)))
                         (return-from word word))
                     (vector-push-extend #\Space word))))))

(defmethod parse ((f files) delimiter &key multiline? forth-values?)
  (with-slots (source-id >in buffer) f
    (let ((parsed (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
          (whitespace-delimiter-p (and (char-equal delimiter #\Space) (plusp source-id))))
      (loop with starting->in = >in
            with address = (when forth-values?
                             (multiple-value-bind (address count)
                                 (access-source-buffer f)
                               (declare (ignore count))
                               (make-address (address-prefix address) (+ (address-address address)
                                                                         (* starting->in +char-size+)))))
            for buffer-len = (length buffer)
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
                 (if forth-values?
                     (return-from parse (values address (* (- >in starting->in 1) +char-size+)))
                     (return-from parse parsed))
            else
              do (if (if (and (plusp source-id) multiline?)
                         (null (refill f))
                         t)
                     (if forth-values?
                         (return-from parse (values address (* (- >in starting->in) +char-size+)))
                         (return-from parse parsed))
                     (vector-push-extend #\Space parsed))))))

(defparameter *escape-translations*
  '((#\a #\Bel)
    (#\b #\Backspace)
    (#\e #\Escape)
    (#\f #\Formfeed)
    (#\l #\Linefeed)
    (#\m (#\Return #\Linefeed))
    (#\n #\Newline)
    (#\q #\")
    (#\r #\Return)
    (#\t #\Tab)
    (#\v #\VT)
    (#\z #\Null)
    (#\" #\")
    (#\\ #\\)))

;;; Special simplified parsing method used only by S\" --
;;;  The delimiter is always double quote (")
;;;  Recognizes the escape sequences defined in Section 6.2.2266 of the Forth 2012 specification
(defmethod escaped-parse ((f files))
  (with-slots (>in buffer) f
    (let ((parsed (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
      (loop with end = (length buffer)
            with translation = nil
            with hex = nil
            while (< >in end)
            do (let ((ch (aref buffer >in)))
                 (cond ((char-equal ch #\")
                        (incf >in)
                        (return-from escaped-parse parsed))
                       ((and (char-equal ch #\\) (< >in (1- end)))
                        (let ((escapee (aref buffer (1+ >in))))
                          (cond ((and (char-equal escapee #\x) (< >in (- end 3))
                                      (setf hex (ignore-errors
                                                 (parse-integer buffer :start (+ >in 2) :end (+ >in 4) :radix 16))))
                                 (vector-push-extend (code-char hex) parsed)
                                 (incf >in 4))
                                ;; Forth 2012 states that the escape characters are case sensitive
                                ;;  (I.e. '\a' is recognized while '\A' is not)
                                ((setf translation (cadr (find escapee *escape-translations* :key #'car)))
                                 (if (listp translation)
                                     (loop for char in translation
                                           do (vector-push-extend char parsed))
                                     (vector-push-extend translation parsed))
                                 (incf >in 2))
                                (t
                                 ;; If the escape sequence isn't recognized, just add it as raw text
                                 (vector-push-extend #\\ parsed)
                                 (vector-push-extend escapee parsed)
                                 (incf >in 2)))))
                       (t
                        (vector-push-extend ch parsed)
                        (incf >in))))
            finally (return parsed)))))

(defmethod refill ((f files))
  (with-slots (source-id >in buffer verbose source-id-map source-as-space) f
    (flet ((fillup (stream)
             (setf (source-data-space-is-valid? source-as-space) nil
                   (source-data-space-buffer source-as-space) nil)
             (let ((line (read-line stream nil :eof)))
               (unless (eq line :eof)
                 (setf buffer line
                       (source-data-space-buffer source-as-space) line
                       >in 0)
                 (when (and verbose (plusp source-id))
                   (write-line buffer))
                 t))))
      (cond ((= source-id -1) nil)
            ((zerop source-id)
             (fillup *standard-input*))
            (t
             (fillup (file-file-stream source-id source-id-map)))))))

(defmethod source-push ((f files) &key fileid evaluate ((:source-address source-address-override)))
  (assert (not (and fileid evaluate)) () "Pass ~S or ~S but not both to ~S" :fileid :evaluate 'source-push)
  (with-slots (source-id >in buffer source-address source-stack source-as-space) f
    (let ((ss (make-saved-source :id source-id :>in >in :buffer buffer :source-address source-address)))
      (stack-push source-stack ss)
      ;; SOURCE-ID for EVALUATE is always -1
      (setf source-id (or fileid -1)
            >in 0
            buffer (or evaluate "")
            ;; SOURCE should return the user's buffer address for EVALUATE
            source-address source-address-override)
      (setf (source-data-space-is-valid? source-as-space) nil
            ;; For EVALUATE, set the buffer in the psuedo space here as the recursive interpreter won't
            (source-data-space-buffer source-as-space) evaluate))))

(defmethod source-pop ((f files))
  (with-slots (source-id >in buffer source-address source-stack source-as-space) f
    (when (plusp source-id)
      (forth-close-file f source-id))
    (let ((ss (stack-pop source-stack)))
      (setf source-id (saved-source-id ss)
            >in (saved-source->in ss)
            buffer (saved-source-buffer ss)
            source-address (saved-source-source-address ss)
            (source-data-space-is-valid? source-as-space) nil
            (source-data-space-buffer source-as-space) (saved-source-buffer ss)))))

(defmethod access-source-buffer ((f files))
  (with-slots (source-address source-as-space) f
    (unless (source-data-space-is-valid? source-as-space)
      (update-source-data-space source-as-space))
    (values (or source-address (make-address (space-prefix source-as-space) 0)) (space-high-water-mark source-as-space))))

(defmethod note-included-file ((f files) wls fileid)
  (with-slots (source-id-map included-files) f
    (let ((file (gethash fileid source-id-map)))
      (when file
        (let* ((included-file (truename (file-%stream file)))
               (included? (gethash included-file included-files)))
          (unless included?
            (note-new-included-file wls included-file)
            (setf (gethash included-file included-files) t)))))))

(defmethod forget-included-file ((f files) included-file)
  (with-slots (included-files) f
    (remhash included-file included-files)))

;;;

(defmethod save-buffer-for-restore ((f files))
  (with-slots (buffer saved-buffer-space) f
    (let* ((count (length buffer))
           (address (space-allocate saved-buffer-space count)))
      (multiple-value-bind (data offset)
          (space-decode-address saved-buffer-space (address-address address) count)
        (native-into-forth-string buffer data offset))
      (values address count))))

(defmethod restore-buffer ((f files) address count)
  (with-slots (buffer saved-buffer-space) f
    (multiple-value-bind (data offset)
        (space-decode-address saved-buffer-space (address-address address) count)
      (setf buffer (forth-string-to-native data offset count)))))

(defmethod save-input ((f files) &key for-catch?)
  (with-slots (source-id >in source-address source-id-map source-stack) f
    (let ((state-vector (make-array 16 :fill-pointer 0 :adjustable t)))
      (vector-push-extend source-id state-vector)
      (vector-push-extend >in state-vector)
      (cond ((= source-id -1)
             ;; EVALUATE supplies the address of the input buffer which we'll use to verify the subsequent restore
             (vector-push-extend source-address state-vector))
            ((zerop source-id)
             ;; For console input, save the current buffer so we can restore it later after a THROW
             (when for-catch?
               (multiple-value-bind (address count)
                   (save-buffer-for-restore f)
                 (vector-push-extend address state-vector)
                 (vector-push-extend count state-vector))))
            ((plusp source-id)
             (let ((stream (file-file-stream source-id source-id-map)))
               (when stream
                 (vector-push-extend (file-position stream) state-vector)
                 ;; For a file input source, save the current input buffer so we can restore it later
                 ;; We can't just reposition the file's stream and read the line back into the buffer as the stream
                 ;; is positioned after the line which is in the input buffer
                 (multiple-value-bind (address count)
                     (save-buffer-for-restore f)
                   (vector-push-extend address state-vector)
                   (vector-push-extend count state-vector))))))
      (when for-catch?
        (vector-push (stack-depth source-stack) state-vector))
      state-vector)))

(defmethod restore-input ((f files) state-vector &key for-throw?)
  (with-slots (source-id >in buffer source-address source-id-map source-stack) f
    (let ((saved-source-id (aref state-vector 0))
          (saved->in (aref state-vector 1)))
      (unless (or (= source-id saved-source-id) for-throw?)
        (forth-exception :save-restore-input-mismatch))
      (setf source-id saved-source-id)
      (prog1
          (cond ((= saved-source-id -1)
                 (let ((saved-source-address (aref state-vector 2)))
                   (unless (or (= source-address saved-source-address) for-throw?)
                     (forth-exception :save-restore-input-mismatch))
                   (setf >in saved->in)
                   t))
                ((zerop saved-source-id)
                 (when for-throw?
                   (let ((saved-buffer-address (aref state-vector 2))
                         (saved-buffer-count (aref state-vector 3)))
                     (restore-buffer f saved-buffer-address saved-buffer-count)
                     (setf >in saved->in))))
                (t
                 (let ((stream (file-file-stream saved-source-id source-id-map))
                       (saved-file-position (aref state-vector 2))
                       (saved-buffer-address (aref state-vector 3))
                       (saved-buffer-count (aref state-vector 4)))
                   (when stream
                     (handler-case
                         (progn
                           (file-position stream saved-file-position)
                           (restore-buffer f saved-buffer-address saved-buffer-count)
                           (setf >in saved->in)
                           t)
                       (file-error () nil))))))
        (when for-throw?
          (let ((target-depth (aref state-vector (1- (length state-vector)))))
            (loop while (> (stack-depth source-stack) target-depth)
                  do (let* ((source (stack-pop source-stack))
                            (saved-source-id (saved-source-id source)))
                       (when (and (plusp saved-source-id) (/= saved-source-id source-id))
                         (forth-close-file f (saved-source-id source)))))))))))


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
              'character)
          (logtest fam +binary-mode+)))

;;; Forth's definition of OPEN-FILE specifies that the file must exist. It also specifies that the file be positioned
;;; to the beginning of the file. (It doesn't say what should happen if opening for output.)
(defmethod forth-open-file ((f files) pathname fam)
  (with-slots (source-id-map last-source-id) f
    (multiple-value-bind (direction element-type binary?)
        (interpret-file-access-method fam)
      (let ((stream (open pathname :direction direction :element-type element-type :if-exists :append :if-does-not-exist nil)))
        (if stream
            (let ((fileid (incf last-source-id)))
              (setf (gethash fileid source-id-map) (make-file stream binary?))
              (file-position stream 0)
              (values fileid +file-operation-success+))
            (values 0 +file-operation-failure+))))))

;; Forth's definition of CREATE-FILE specifies that it replaces an existing file
(defmethod forth-create-file ((f files) pathname fam)
  (with-slots (source-id-map last-source-id) f
    (multiple-value-bind (direction element-type binary?)
        (interpret-file-access-method fam)
      (let ((stream (open pathname :direction direction :element-type element-type :if-exists :supersede
                                   :if-does-not-exist :create)))
        (if stream
            (let ((fileid (incf last-source-id)))
              (setf (gethash fileid source-id-map) (make-file stream binary?))
              (values fileid +file-operation-success+))
            (values 0 +file-operation-failure+))))))

(defmethod forth-delete-file ((f files) pathname)
  (handler-case
      (progn
        (delete-file pathname)
        +file-operation-success+)
    (file-error () +file-operation-failure+)))

(defmethod forth-rename-file ((f files) old-pathname new-pathname)
  (handler-case
      (progn
        (rename-file old-pathname new-pathname)
        +file-operation-success+)
    (file-error () +file-operation-failure+)))
  
(defmethod forth-file-status ((f files) pathname)
  (handler-case
      (let ((probe (probe-file pathname)))
        (if probe
            (values +true+ +file-operation-success+)
            (values +false+ +file-operation-failure+)))
    (file-error () (values +false+ +file-operation-failure+))))

(defmethod forth-close-file ((f files) fileid)
  (with-slots (source-id-map) f
    (let* ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (remhash fileid source-id-map)
      (handler-case
          (progn
            (close stream)
            +file-operation-success+)
        (file-error () +file-operation-failure+)))))

(defmethod forth-file-position ((f files) fileid)
  (with-slots (source-id-map) f
    (let ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (handler-case
          (values (file-position stream) +file-operation-success+)
        (file-error () (values 0 +file-operation-failure+))))))

(defmethod forth-file-reposition ((f files) fileid position)
  (with-slots (source-id-map) f
    (let ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (handler-case
          (let ((success (file-position stream position)))
            (if success +file-operation-success+ +file-operation-failure+))
        (file-error () +file-operation-failure+)))))

(defmethod forth-file-size ((f files) fileid)
  (with-slots (source-id-map) f
    (let ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (handler-case
          (values (file-length stream) +file-operation-success+)
        (file-error () (values 0 +file-operation-failure+))))))

(defmethod forth-read-file ((f files) fileid buffer offset count)
  (with-slots (source-id-map) f
    (let* ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID")))
           (file (gethash fileid source-id-map)))
      (cond  ((file-binary? file)
              (handler-case
                  (let* ((end (+ offset count))
                         (position (read-sequence buffer stream :start offset :end end)))
                    (values (- position offset) +file-operation-success+))
                (file-error () (values 0 +file-operation-failure+))))
             (t
              (let ((data (make-array count :element-type 'character :initial-element #\Null :fill-pointer count))
                    (updated-offset offset)
                    (updated-count count)
                    (read-count 0))
                (when (file-buffer file)
                  ;; For a charaacter file, previous call(s) to READ-LINE may read a line that was too large than the user's
                  ;; buffer. If so, the remainder of  was saved in the FILE-BUFFER slot. We'll return that and, if needed,
                  ;; keep reading to satisfy the requested COUNT
                  (let ((amount (min updated-count (length (file-buffer file)))))
                    (replace data (file-buffer file) :start1 0 :end1 amount :start2 0 :end2 amount)
                    (when (> (length (file-buffer file)) amount)
                      (setf (file-buffer file) (subseq (file-buffer file) amount)))
                    (incf updated-offset amount)
                    (decf updated-count amount)
                    (setf read-count amount)))
                (when (plusp updated-count)
                  ;; Still have room left to read more of the file ...
                  (handler-case
                      (let* ((end (+ updated-offset updated-count))
                             (position (read-sequence data stream :start updated-offset :end end)))
                        (setf read-count (- position offset)))
                    (file-error ()
                      (return-from forth-read-file (values 0 +file-operation-failure+)))))
                ;; Convert the buffer to a Forth string
                (setf (fill-pointer data) read-count)
                (native-into-forth-string data buffer offset)
                (values read-count +file-operation-success+)))))))
                  

(defmethod forth-read-line ((f files) fileid buffer-size)
  (with-slots (source-id-map) f
    (let* ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID")))
           (file (gethash fileid source-id-map)))
      ;; Previous call(s) to READ-LINE may read a line that was too large than the user's buffer. If so, the remainder of
      ;; that line was saved in the FILE-BUFFER slot. We'll return that rather than reading another line from the file.
      (let ((line (or (file-buffer file)
                      (handler-case
                          (read-line stream nil :eof)
                        (file-error () (values "" nil +file-operation-failure+))))))
        (if (eq line :eof)
            (values "" t +file-operation-success+)
            (let ((line-size (length line)))
              (if (> line-size buffer-size)
                  (setf (file-buffer file) (subseq line buffer-size)
                        line (subseq line 0 buffer-size))
                  (setf (file-buffer file) nil))
              (values line nil +file-operation-success+)))))))
  
(defmethod forth-write-file ((f files) fileid buffer offset count)
  (with-slots (source-id-map) f
    (let* ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID")))
           (file (gethash fileid source-id-map)))
      (cond  ((file-binary? file)
              (handler-case
                  (let ((end (+ offset count)))
                    (write-sequence buffer stream :start offset :end end)
                    +file-operation-success+)
                (file-error () +file-operation-failure+)))
             (t
              (let ((data (forth-string-to-native buffer offset count)))
                (handler-case
                    (progn
                      (write-sequence data stream)
                      +file-operation-success+)
                  (file-error () +file-operation-failure+))))))))

(defmethod forth-write-line ((f files) fileid line)
  (with-slots (source-id-map) f
    (let ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (handler-case
          (progn
            (write-line line stream)
            +file-operation-success+)
        (file-error () +file-operation-failure+)))))

(defmethod forth-flush-file ((f files) fileid)
  (with-slots (source-id-map) f
    (let ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (handler-case
          (progn
            (force-output stream)
            +file-operation-success+)
        (file-error () +file-operation-failure+)))))

(defmethod forth-file-resize ((f files) fileid size)
  (with-slots (source-id-map) f
    (let ((stream (or (file-file-stream fileid source-id-map) (forth-exception :file-i/o-exception "Invalid FILEID"))))
      (handler-case
          (progn
            (set-stream-length stream size)
            +file-operation-success+)
        (file-error () +file-operation-failure+)))))
