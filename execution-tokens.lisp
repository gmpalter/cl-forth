(in-package #:forth)

(defstruct xt
  (token)
  (word)
  (>body))

(defclass execution-tokens (space)
  ((name-to-xt-map :initform (make-hash-table :test #'equalp))
   (token-to-xt-map :initform (make-hash-table))
   (saved-high-water-mark :initform 0)
   (saved-name-to-xt-map :initform nil)
   (saved-token-to-xt-map :initform nil))
  )

(defmethod register-execution-token ((xts execution-tokens) word here)
  (with-slots (prefix high-water-mark name-to-xt-map token-to-xt-map) xts
    (let ((address (make-address prefix high-water-mark)))
      (incf high-water-mark +cell-size+)
      (let ((xt (make-xt :token address :word word :>body here)))
        (when (word-name word)
          (setf (gethash (word-name word) name-to-xt-map) xt))
        (setf (gethash address token-to-xt-map) xt))
      address)))

(defmethod find-xt ((xts execution-tokens) name)
  (with-slots (name-to-xt-map) xts
    (let ((xt (gethash name name-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "No execution token for ~A" name))
      (xt-token xt))))

(defmethod find-xt-and-word ((xts execution-tokens) name)
  (with-slots (name-to-xt-map) xts
    (let ((xt (gethash name name-to-xt-map)))
      (when xt
        (values (xt-token xt) (xt-word xt))))))

(defmethod execute ((xts execution-tokens) token fs)
  (with-slots (token-to-xt-map) xts
    (let ((xt (gethash token token-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "~14,'0X is not the address of an execution token" token))
      (forth-call fs (xt-word xt)))))

(defmethod find-body ((xts execution-tokens) token)
  (with-slots (token-to-xt-map) xts
    (let ((xt (gethash token token-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "~14,'0X is not the address of an execution token" token))
      (xt->body xt))))

(defmethod space-reset ((sp execution-tokens))
  (with-slots (high-water-mark name-to-xt-map token-to-xt-map
               saved-high-water-mark saved-name-to-xt-map saved-token-to-xt-map) sp
    (setf high-water-mark saved-high-water-mark
          name-to-xt-map (or saved-name-to-xt-map (make-hash-table :test #'equalp))
          token-to-xt-map (or saved-token-to-xt-map (make-hash-table))))
  nil)

(defmethod save-space-state ((xts execution-tokens))
  (with-slots (high-water-mark name-to-xt-map token-to-xt-map
               saved-high-water-mark saved-name-to-xt-map saved-token-to-xt-map) sp
    (setf saved-high-water-mark high-water-mark
          saved-name-to-xt-map name-to-xt-map
          saved-token-to-xt-map token-to-xt-map))
  nil)

;;; Prevent accidental access

(defmethod space-allocate ((xts execution-tokens) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :invalid-memory))

(defmethod space-align ((xts execution-tokens))
  nil)

(defmethod cell-at ((xts execution-tokens) address)
  (declare (ignore address))
  (forth-exception :invalid-memory))

(defmethod (setf cell-at) (value (xts execution-tokens) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod cell-unsigned-at ((xts execution-tokens) address)
  (declare (ignore address))
  (forth-exception :invalid-memory))

(defmethod (setf cell-unsigned-at) (value (xts execution-tokens) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod byte-at ((xts execution-tokens) address)
  (declare (ignore address))
  (forth-exception :invalid-memory))

(defmethod (setf byte-at) (value (xts execution-tokens) address)
  (declare (ignore value address))
  (forth-exception :write-to-read-only-memory))

(defmethod space-fill ((xts execution-tokens) address count byte)
  (declare (ignore address count byte))
  (forth-exception :write-to-read-only-memory))

(defmethod space-copy ((ssp execution-tokens) source-address (dsp space) destination-address count)
  (declare (ignore source-address destination-address count))
  (forth-exception :invalid-memory))

(defmethod space-copy ((ssp space) source-address (dsp execution-tokens) destination-address count)
  (declare (ignore source-address destination-address count))
  (forth-exception :write-to-read-only-memory))

(defmethod space-decode-address ((xts execution-tokens) address)
  (declare (ignore address))
  (forth-exception :invalid-memory))
