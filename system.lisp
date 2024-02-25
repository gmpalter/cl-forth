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
  
(defmethod state ((fs forth-system))
  (with-slots (state) fs
    (if (zerop state)
        :interpreting
        :compiling)))

(defmethod (setf state) (value (fs forth-system))
  (with-slots (state) fs
    (setf state (ecase value
                  (:interpreting 0)
                  (:compiling 1))))
  value)
         
(defmacro with-forth-system ((fs) &body body)
  `(with-slots (memory data-stack return-stack control-flow-stack float-stack
                word-lists files base state)
       ,fs
     ,@body))

(defmacro define-forth-method (name (fs &rest args) &body body)
  `(defmethod ,name ((,fs forth-system) ,@args)
     (with-forth-system (,fs)
       ,@body)))

(define-forth-method reset-interpreter/compiler (fs)
  (stack-reset data-stack)
  (stack-reset return-stack)
  (stack-reset control-flow-stack)
  (stack-reset float-stack)
  (reset-input files)
  (setf (state fs) :interpreting)
  )

(define-forth-method toplevel (fs)
  (reset-interpreter/compiler fs)
  (catch 'bye
    (loop
      (handler-case
          (interpreter/compiler fs)
        (forth-error (e)
          (unless (eq (forth-error-key e) :quit)
            (write-line (forth-error-phrase e)))
          (reset-interpreter/compiler fs))))))

(define-forth-method interpreter/compiler (fs)
  (loop with first = t
    do (loop for empty = t then nil
             while (input-available-p files)
             as token = (word files #\Space)
             do (multiple-value-bind (type value)
                    (let ((word (lookup word-lists token)))
                      (if word
                          (values :word word)
                          (interpret-number token base)))
                  (when (null type)
                    (forth-error :undefined-word "~A is not defined" token))
                  (case (state fs)
                    (:interpreting
                     (case type
                       (:word
                        (cond ((word-compile-only? value)
                               (forth-error :compile-only-word))
                              (t
                               (forth-call fs value))))
                       (:single
                        (stack-push data-stack value))
                       (:double
                        (stack-push-double data-stack value))
                       (:float
                        (stack-push float-stack value))))
                    (:compiling
                     #+not-yet
                     (case type
                       (:word
                        (cond ((word-immediate? value)
                               (forth-call fs value))
                              ((word-inlineable? value)
                               (setf forms (append (word-inline-forms value) forms)))
                              (t
                               (push `(forth-call fs ,value) forms))))
                       (:single
                        (push `(stack-push data-stack ,value) forms))
                       (:double
                        (push `(stack-push-double data-stack ,value) forms))
                       (:float
                        (push `(stack-push float-stack ,value) forms))))))
             finally
                (when (and (terminal-input-p files) (not (shiftf first nil)) (not empty))
                  (write-line "OK.")))
       (unless (refill files)
         (if (terminal-input-p files)
             (throw 'bye nil)
             (source-pop files)))))

(define-forth-method begin-compilation (fs)
  )

(define-forth-method finish-compilation (fs)
  )

(defun forth-call (fs word)
  (with-forth-system (fs)
    (let ((return-address (multiple-value-bind (fn pc) (ccl::cfp-lfun (ccl::%get-frame-ptr))
                            (+ (%address-of fn) pc))))
      (stack-push return-stack return-address)
      (unwind-protect
           (apply (word-code word) fs (word-parameters word))
        (stack-pop return-stack)))))
