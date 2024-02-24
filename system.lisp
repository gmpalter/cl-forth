(in-package #:forth)

(defclass forth-system ()
  ((memory :initform (make-instance 'memory))
   (data-stack :initform (make-instance 'stack :initial-size 1024
                                               :underflow-key :stack-underflow :overflow-key :stack-overflow))
   (return-stack :initform (make-instance 'stack :initial-size 128
                                                 :underflow-key :return-stack-underflow :overflow-key :return-stack-overflow))
   (control-flow-stack :initform (make-instance 'stack :initial-size 128
                                                       :underflow-key :control-flow-stack-underflow
                                                       :overflow-key :control-flow-stack-overflow))
   (float-stack :initform (make-instance 'stack :initial-size 32
                                                :underflow-key :float-stack-underflow :overflow-key :float-stack-overflow))
   (word-lists :initform (make-instance 'word-lists))
   (files :initform (make-instance 'files))
   (base :initform 10)
   state)
  )

(defmethod initialize-instance :after ((fs forth-system) &key &allow-other-keys)
  (with-slots (memory word-lists files) fs
    (add-state-space memory fs)
    (add-state-space memory word-lists)
    (add-state-space memory files)
    (install-predefined-words word-lists)
    ))
  
(defmacro with-forth-system ((fs) &body body)
  `(with-slots (memory data-stack return-stack control-flow-stack float-stack
                word-lists files base state)
       ,fs
     ,@body))

(defmacro define-forth-method (name (fs &rest args) &body body)
  `(defmethod ,name ((,fs forth-system) ,@args)
     (with-forth-system (,fs)
       ,@body)))

(define-forth-method reset-interpreter (fs)
  (stack-reset data-stack)
  (stack-reset return-stack)
  (stack-reset control-flow-stack)
  (stack-reset float-stack)
  (reset-input files)
  (setf state 0)
  )

(define-forth-method toplevel (fs)
  (reset-interpreter fs)
  (catch 'bye
    (loop
      (handler-case
          (interpreter fs)
        (forth-error (e)
          (unless (eq (forth-error-key e) :quit)
            (write-line (forth-error-phrase e)))
          (reset-interpreter fs))))))

;;;---*** TODO: Rethink INTERPRETER vs. COMPILER methods

(define-forth-method interpreter (fs)
  (loop with first = t
    do (loop for empty = t then nil
             while (input-available-p files)
             do (let* ((thing (word files #\Space))
                       (word (lookup word-lists thing)))
                  (cond (word
                         (cond ((word-compile-only? word)
                                (forth-error :compile-only-word))
                               (t
                                (forth-call fs word))))
                        (t
                         (multiple-value-bind (type value)
                             (interpret-number thing base)
                           (case type
                             (:single
                              (stack-push data-stack value))
                             (:double
                              (stack-push-double data-stack value))
                             (:float
                              (stack-push float-stack value))
                             (otherwise
                              (forth-error :undefined-word "~A is not defined" thing)))))))
             finally
                (when (and (terminal-input-p files) (not (shiftf first nil)) (not empty))
                  (write-line "OK.")))
       (unless (refill files)
         (if (terminal-input-p files)
             (throw 'bye nil)
             (return-from interpreter nil)))))

(define-forth-method compiler (fs)
  (when pending-definition
    (forth-error :recursive-compile))
  (loop
    do (loop while (input-available-p files)
             do (let* ((thing (word files #\Space))
                       (word (lookup word-lists thing)))
                  (cond (word
                         (cond ((word-immediate? word)
                                (forth-call fs word))
                               (t
                                (push `(forth-call fs ,word) forms))))
                        (t
                         (multiple-value-bind (type value)
                             (interpret-number thing base)
                           (case type
                             (:single
                              (push `(stack-push data-stack ,value) forms))
                             (:double
                              (push `(stack-push-double data-stack ,value) forms))
                             (:float
                              (push `(stack-push float-stack ,value) forms))
                             (otherwise
                              (forth-error :undefined-word "~A is not defined" thing))))))))
       (unless (refill files)
         (forth-error :unexpected-eof "Incomplete definition"))))

(defun forth-call (fs word)
  (with-forth-system (fs)
    (let ((return-address (multiple-value-bind (fn pc) (ccl::cfp-lfun (ccl::%get-frame-ptr))
                            (+ (%address-of fn) pc))))
      (stack-push return-stack return-address)
      (unwind-protect
           (apply (word-code word) fs (word-parameters word))
        (stack-pop return-stack)))))
