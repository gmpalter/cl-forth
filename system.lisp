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
   state
   (compiling-word :initform nil)
   (compiling-paused? :initform nil))
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
                word-lists files base state compiling-word compiling-paused?)
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
  (setf compiling-word nil)
  (setf compiling-paused? nil)
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
                     (case type
                       (:word
                        (cond ((word-immediate? value)
                               (forth-call fs value))
                              ((word-inlineable? value)
                               (setf (word-inline-forms compiling-word)
                                     (append (word-inline-forms value) (word-inline-forms compiling-word))))
                              (t
                               (push `(forth-call fs ,value) (word-inline-forms compiling-word)))))
                       (:single
                        (push `(stack-push data-stack ,value) (word-inline-forms compiling-word)))
                       (:double
                        (push `(stack-push-double data-stack ,value) (word-inline-forms compiling-word)))
                       (:float
                        (push `(stack-push float-stack ,value) (word-inline-forms compiling-word)))))))
             finally
                (when (and (terminal-input-p files) (not (shiftf first nil)) (not empty))
                  (write-line "OK.")))
       (unless (refill files)
         (if (terminal-input-p files)
             (throw 'bye nil)
             (source-pop files)))))

(define-forth-method begin-compilation (fs &optional name)
  (unless (eq (state fs) :interpreting)
    (forth-error :recursive-compile))
  (setf compiling-word (make-word name nil :smudge? t)
        compiling-paused? nil)
  ;; :NONAME creates a word without a name and places its "execution token" on the data stack
  (when name
    (add-word (word-lists-compilation-word-list word-lists) compiling-word))
  (setf (state fs) :compiling))

(define-forth-method finish-compilation (fs)
  (unless (eq (state fs) :compiling)
    (forth-error :not-compiling))
  (let ((thunk `(lambda (fs &rest parameters)
                  (declare (ignorable parameters))
                  (with-forth-system (fs)
                    ,@(reverse (word-inline-forms compiling-word))))))
    (setf (word-code compiling-word) (compile nil thunk)))
  (setf (word-inline-forms compiling-word) nil)
  (setf (word-smudge? compiling-word) nil)
  ;; Leave the new definition in COMPILING-WORD for use by IMMEDIATE
  (setf compiling-paused? nil)
  (setf (state fs) :interpreting))

(defun forth-call (fs word)
  (with-forth-system (fs)
    (let ((return-address (multiple-value-bind (fn pc) (ccl::cfp-lfun (ccl::%get-frame-ptr))
                            (+ (%address-of fn) pc))))
      (stack-push return-stack return-address)
      (unwind-protect
           (apply (word-code word) fs (word-parameters word))
        (stack-pop return-stack)))))
