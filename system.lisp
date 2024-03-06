(in-package #:forth)

(defclass forth-system ()
  ((memory :initform (make-instance 'memory))
   (data-stack :initform (make-instance 'stack :name "Data" :initial-size 1024
                                               :underflow-key :stack-underflow :overflow-key :stack-overflow))
   (return-stack :initform (make-instance 'stack :name "Return" :initial-size 128
                                                 :underflow-key :return-stack-underflow :overflow-key :return-stack-overflow))
   (control-flow-stack :initform (make-instance 'stack :name "Control-flow"
                                                       :initial-size 128
                                                       :underflow-key :control-flow-stack-underflow
                                                       :overflow-key :control-flow-stack-overflow))
   (float-stack :initform (make-instance 'stack :name "Float"
                                                :initial-size 32
                                                :underflow-key :float-stack-underflow :overflow-key :float-stack-overflow))
   (word-lists :initform (make-instance 'word-lists))
   (files :initform (make-instance 'files))
   (execution-tokens :initform (make-instance 'execution-tokens))
   (base :initform 10)
   state
   (compiling-word :initform nil)
   (compiling-paused? :initform nil)
   (exit-branch :initform nil)
   (>body-address :initform 0)
   (show-redefinition-warnings? :initform +true+)
   (reset-redefinition-warnings? :initform nil)
   (show-definition-code? :initform +false+))
  )

(defmethod initialize-instance :after ((fs forth-system) &key &allow-other-keys)
  (with-slots (memory word-lists files execution-tokens) fs
    (add-state-space memory fs)
    (add-state-space memory word-lists)
    (add-state-space memory files)
    (add-space memory (files-source-as-space files))
    (add-space memory execution-tokens)
    (register-predefined-words word-lists execution-tokens (data-space-high-water-mark memory))
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
                word-lists files execution-tokens base state compiling-word compiling-paused?
                >body-address exit-branch show-redefinition-warnings? reset-redefinition-warnings?
                show-definition-code?)
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
  (reset-pictured-buffer memory)
  (setf (state fs) :interpreting)
  (setf compiling-word nil)
  (setf compiling-paused? nil)
  )

(define-forth-method toplevel (fs &key evaluate)
  (reset-interpreter/compiler fs)
  (when evaluate
    (source-push files :evaluate evaluate))
  (catch 'bye
    (loop
      (restart-case
          (handler-case
              (interpreter/compiler fs)
            (forth-exception (e)
              (unless (member (forth-exception-key e) '(:abort :quit))
                (write-line (forth-exception-phrase e)))
              (clear-input)
              (reset-interpreter/compiler fs)))
        (abort () :report (lambda (stream) (write-string "Return to FORTH toplevel" stream))
          (reset-interpreter/compiler fs))))))

(define-forth-method interpreter/compiler (fs &key (toplevel? t))
  (loop with first = t
    do (loop for empty = t then nil
             while (input-available-p files)
             as token = (word files #\Space)
             ;; If there's more than one whitespace character at the end of a line, WORD will return a null TOKEN
             when token
               do (multiple-value-bind (type value)
                      (let ((word (lookup word-lists token)))
                        (if word
                            (values :word word)
                            (interpret-number token base)))
                    (when (null type)
                      (forth-exception :undefined-word "~A is not defined" token))
                    (case (state fs)
                      (:interpreting
                       (case type
                         (:word
                          (cond ((word-compile-only? value)
                                 (forth-exception :compile-only-word))
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
                                       (append (reverse (word-inline-forms value)) (word-inline-forms compiling-word))))
                                (t
                                 (push `(forth-call fs ,value) (word-inline-forms compiling-word)))))
                         (:single
                          (push `(stack-push data-stack ,value) (word-inline-forms compiling-word)))
                         (:double
                          (push `(stack-push-double data-stack ,value) (word-inline-forms compiling-word)))
                         (:float
                          (push `(stack-push float-stack ,value) (word-inline-forms compiling-word)))))))
             finally
                (when (and (eq (state fs) :interpreting) (terminal-input-p files) (not (shiftf first nil)) (not empty))
                  (write-line "OK.")))
       (unless (refill files)
         (cond ((not toplevel?)
                (source-pop files)
                (return-from interpreter/compiler nil))
               ((terminal-input-p files)
                (throw 'bye nil))
               (t
                (source-pop files))))))

(define-forth-method begin-compilation (fs &optional name)
  (unless (eq (state fs) :interpreting)
    (forth-exception :recursive-compile))
  (setf compiling-word (make-word name nil :smudge? t)
        compiling-paused? nil
        exit-branch (make-branch-reference :exit))
  (setf (state fs) :compiling)
  (if name
      (add-and-register-word fs compiling-word)
      ;; :NONAME creates a word without a name and places its "execution token" on the data stack
      (register-execution-token execution-tokens compiling-word >body-address)))

(define-forth-method add-and-register-word (fs word)
  (add-word (word-lists-compilation-word-list word-lists) word :silent (falsep show-redefinition-warnings?))
  ;; Ensure that IMMEDIATE will affect this word if it wasn't created by BEGIN-COMPILATION (e.g., by CREATE and friends)
  (setf compiling-word word)
  (register-execution-token execution-tokens word (data-space-high-water-mark memory)))

(define-forth-method finish-compilation (fs)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (unless (zerop (stack-depth control-flow-stack))
    (forth-exception :control-mismatch))
  (let ((thunk `(lambda (fs &rest parameters)
                  (declare (ignorable parameters))
                  (with-forth-system (fs)
                    (tagbody
                       ,@(reverse (word-inline-forms compiling-word))
                       ,(branch-reference-tag exit-branch))))))
    (when (not (zerop show-definition-code?))
      (format t "~&Code for ~A:~%  ~:W~%" (or (word-name compiling-word) "<execution token>") thunk))
    (setf (word-code compiling-word) (compile nil thunk)))
  (setf (word-inline-forms compiling-word) nil)
  (setf (word-smudge? compiling-word) nil)
  ;; Leave the new definition in COMPILING-WORD for use by IMMEDIATE
  (setf compiling-paused? nil
        exit-branch nil
        >body-address nil)
  (when (shiftf reset-redefinition-warnings? nil)
    (setf show-redefinition-warnings? +true+))
  (setf (state fs) :interpreting))

(define-forth-method postpone (fs word)
  (cond ((word-immediate? word)
         (push `(forth-call fs ,word) (word-inline-forms compiling-word)))
        (t
         (push `(case (state fs)
                  (:interpreting
                   (forth-call fs ,word))
                  (:compiling
                   (push '(forth-call fs ,word) (word-inline-forms compiling-word))))
               (word-inline-forms compiling-word)))
        ;;---*** NOTE: I don't know under what circumstances POSTPONE should produce this error.
        ;;(t
        ;; (forth-exception :invalid-postpone))
        ))

(defun forth-call (fs word)
  (with-forth-system (fs)
    (let ((return-address (multiple-value-bind (fn pc) (ccl::cfp-lfun (ccl::%get-frame-ptr))
                            (+ (%address-of fn) pc))))
      (stack-push return-stack return-address)
      (unwind-protect
           (apply (word-code word) fs (word-parameters word))
        (stack-pop return-stack)))))

;;;

(defstruct (branch-reference (:constructor %make-branch-reference))
  type
  tag)

(declaim (inline make-branch-reference))
(defun make-branch-reference (type)
  (%make-branch-reference :type type :tag (gensym (symbol-name type))))

(define-forth-method verify-control-structure (fs type &optional (n 1))
  (when (zerop (stack-depth control-flow-stack))
    (forth-exception :control-mismatch))
  (unless (loop for i below n
                always (eq (branch-reference-type (stack-cell control-flow-stack i)) type))
    (forth-exception :control-mismatch)))

(define-forth-method control-structure-push (fs branch)
  (stack-push control-flow-stack branch))

(define-forth-method control-structure-find (fs type &optional (n 0))
  "Find the Nth TYPE entry on the control stack and return it, where N=0 is the most recent entry, etc."
  (let* ((count 0)
         (position (stack-find-if #'(lambda (cell) (when (eq (branch-reference-type cell) type)
                                                     (if (= count n)
                                                         t
                                                         (progn (incf count) nil))))
                                  control-flow-stack)))
    (if position
        (stack-cell control-flow-stack position)
        (forth-exception :control-mismatch))))

(define-forth-method control-structure-pop (fs type)
  "Find the most recent TYPE entry on the control stack, remove it from the stack, and return it"
  (let ((n (stack-find-if #'(lambda (cell) (eq (branch-reference-type cell) type)) control-flow-stack)))
    (if n
        (stack-snip control-flow-stack n)
        (forth-exception :control-mismatch))))

(define-forth-method execute-branch (fs branch &optional condition)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (if condition
      (push `(when ,condition
               (go ,(branch-reference-tag branch)))
            (word-inline-forms compiling-word))
      (push `(go ,(branch-reference-tag branch)) (word-inline-forms compiling-word)))
  nil)

(define-forth-method resolve-branch (fs branch)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (push (branch-reference-tag branch) (word-inline-forms compiling-word))
  nil)

