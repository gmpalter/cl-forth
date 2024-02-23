(in-package #:forth)

(defclass forth-system ()
  ((memory :initform (make-instance 'memory))
   (data-stack :initform (make-instance 'stack :initial-size 1024 :underflow-phrase "Stack empty" :underflow-code -4))
   (return-stack :initform (make-instance 'stack :initial-size 128 :underflow-phrase "Return stack empty" :underflow-code -6))
   (control-flow-stack :initform (make-instance 'stack :initial-size 128
                                                       :underflow-phrase "Control-flow stack empty" :underflow-code -102))
   (float-stack :initform (make-instance 'stack :initial-size 32
                                                :underflow-phrase "Floating-point stack empty" :underflow-code -45))
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

(define-condition forth-error (error)
  ((phrase :initarg :phrase :reader forth-error-phrase)
   (code :initarg :code :reader forth-error-code))
  (:report (lambda (fe stream)
             (format stream "Forth error ~D: ~A" (forth-error-code fe) (forth-error-phrase fe)))))

(defun forth-error (phrase code)
  (error 'forth-error :phrase phrase :code code))

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
          (write-line (forth-error-phrase e))
          (reset-interpreter fs))))))

(define-forth-method interpreter (fs)
  (loop with first = t
    do (loop for empty = t then nil
             while (input-available-p files)
             do (let* ((thing (word files #\Space))
                       (word (lookup word-lists thing)))
                  (cond (word
                         (cond ((word-precedence word)
                                (forth-error "Interpreting compile-only word" -14))
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
                              (forth-error "Unknown word" -13)))))))
             finally
                (when (and (terminal-input-p files) (not (shiftf first nil)) (not empty))
                  (write-line "OK.")))
       (unless (refill files)
         (if (terminal-input-p files)
             (throw 'bye nil)
             (return-from interpreter nil)))))

(define-forth-method compiler (fs)
  (when pending-definition
    (forth-error "Compiler nesting" -29))
  (loop
    do (loop while (input-available-p files)
             do (let* ((thing (word files #\Space))
                       (word (lookup word-lists thing)))
                  (cond (word
                         (cond ((word-precedence word)
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
                              (forth-error "Unknown word" -13))))))))
       (unless (refill files)
         (forth-error "Incomplete definition" -39))))

(defun forth-call (fs word)
  (with-forth-system (fs)
    (let ((return-address (multiple-value-bind (fn pc) (ccl::cfp-lfun (ccl::%get-frame-ptr))
                            (+ (%address-of fn) pc))))
      (stack-push return-stack return-address)
      (unwind-protect
           (apply (word-code word) fs (word-parameters word))
        (stack-pop return-stack)))))
