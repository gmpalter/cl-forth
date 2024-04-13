(in-package #:forth)

(defstruct (psuedo-pc (:conc-name #:ppc-)
                      (:constructor %make-psuedo-pc)
                      (:print-function (lambda (ppc stream depth)
                                         (declare (ignore depth))
                                         (format stream "~A|~D" (word-name (ppc-word ppc)) (ppc-call-site ppc)))))
                                                 
  word
  call-site)

(declaim (inline make-psuedo-pc))
(defun make-psuedo-pc (word call-site)
  (%make-psuedo-pc :word word :call-site call-site))

(defparameter *interpreter-psuedo-pc*
  (make-psuedo-pc (make-word "INTERPRETER" nil) 1106))

(defstruct xt
  (token)
  (word)
  (>body))

(defclass execution-tokens (space)
  ((token-to-xt-map :initform (make-hash-table))
   (saved-high-water-mark :initform 0)
   (saved-token-to-xt-map :initform nil))
  )

(defmethod register-execution-token ((xts execution-tokens) word here)
  (with-slots (prefix high-water-mark token-to-xt-map) xts
    (let ((address (make-address prefix high-water-mark)))
      (incf high-water-mark +cell-size+)
      (let ((xt (make-xt :token address :word word :>body here)))
        (setf (gethash address token-to-xt-map) xt)
        (setf (word-execution-token word) xt))
      address)))

(defmethod reregister-execution-token ((xts execution-tokens) xt)
  (with-slots (token-to-xt-map) xts
    (setf (gethash (xt-token xt) token-to-xt-map) xt)))

(defmethod verify-execution-token ((xts execution-tokens) token)
  (with-slots (token-to-xt-map) xts
    (let ((xt (gethash token token-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "~14,'0X is not an execution token" token)))))
  
(defmethod execute ((xts execution-tokens) token fs)
  (with-slots (token-to-xt-map) xts
    (let ((xt (gethash token token-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "~14,'0X is not an execution token" token))
      (forth-call fs (xt-word xt) *interpreter-psuedo-pc*))))

(defmethod find-word ((xts execution-tokens) token)
  (with-slots (token-to-xt-map) xts
    (let ((xt (gethash token token-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "~14,'0X is not an execution token" token))
      (xt-word xt))))

(defmethod find-body ((xts execution-tokens) token)
  (with-slots (token-to-xt-map) xts
    (let ((xt (gethash token token-to-xt-map)))
      (when (null xt)
        (forth-exception :no-execution-token "~14,'0X is not an execution token" token))
      (when (null (word-creating-word? (xt-word xt)))
        (forth-exception :invalid->body))
      (xt->body xt))))

(defmethod delete-execution-token ((xts execution-tokens) word)
  (with-slots (token-to-xt-map) xts
    (remhash (word-execution-token word) token-to-xt-map)))

(defmethod space-reset ((sp execution-tokens))
  (with-slots (high-water-mark token-to-xt-map
               saved-high-water-mark saved-token-to-xt-map) sp
    (setf high-water-mark saved-high-water-mark
          token-to-xt-map (or saved-token-to-xt-map (make-hash-table))))
  nil)

(defmethod save-space-state ((xts execution-tokens))
  (with-slots (high-water-mark token-to-xt-map
               saved-high-water-mark saved-token-to-xt-map) xts
    (setf saved-high-water-mark high-water-mark
          saved-token-to-xt-map token-to-xt-map))
  nil)

;;; Prevent accidental access

(defmethod space-allocate ((xts execution-tokens) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :invalid-memory))

(defmethod space-align ((xts execution-tokens) &optional (boundary +cell-size+))
  (declare (ignore boundary))
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
