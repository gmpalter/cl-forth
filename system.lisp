(in-package #:forth)

(defvar *exception-hook* nil
  "If non-NIL, called after an exception, including ABORT and ABORT\",  to perform additional processing")

(defvar *exception-prefix* nil
  "If non-NIL, display this string before displaying the exception's phrase")

(defvar *exit-hook* nil
  "If non-NIL, called before a non-fatal exit to perform additional processing")

(defconstant +maximum-locals+ 16)

(defstruct local
  name
  symbol
  initialize?)

(defstruct locals
  state
  locals
  forms)

(defstruct definition
  word
  (locals (make-locals :state :none))
  exit-branch
  >body-address
  (call-site 0)
  (in-progress? t))

(defstruct (branch-reference (:constructor %make-branch-reference))
  type
  tag)

(declaim (inline make-branch-reference))
(defun make-branch-reference (type)
  (%make-branch-reference :type type :tag (gensym (symbol-name type))))

(declaim (inline next-psuedo-pc))
(defun next-psuedo-pc (definition)
  (make-psuedo-pc (definition-word definition) (incf (definition-call-site definition))))

(defclass forth-system ()
  ((memory :initform (make-instance 'memory))
   (data-stack :initform (make-instance 'stack :name "Data" :size 1024
                                               :underflow-key :stack-underflow :overflow-key :stack-overflow))
   (return-stack :initform (make-instance 'stack :name "Return" :size 128
                                                 :underflow-key :return-stack-underflow :overflow-key :return-stack-overflow))
   (control-flow-stack :initform (make-instance 'stack :name "Control-flow"
                                                       :size 128
                                                       :underflow-key :control-flow-stack-underflow
                                                       :overflow-key :control-flow-stack-overflow))
   (exception-stack :initform (make-instance 'stack :name "Exception"
                                                    :size 128
                                                    :underflow-key :exception-stack-underflow
                                                    :overflow-key :exception-stack-overflow))
   (loop-stack :initform (make-instance 'stack :name "Loop Control"
                                               :size 32
                                               :underflow-key :loop-stack-underflow :overflow-key :do-loops-nesting))
   (float-stack :initform (make-instance 'stack :name "Float"
                                                :size 32
                                                :underflow-key :float-stack-underflow :overflow-key :float-stack-overflow))
   (definitions-stack :initform (make-instance 'stack :name "Definitions"
                                                      :size 32
                                                      :underflow-key :definitions-stack-underflow
                                                      :overflow-key :definitions-stack-overflow))
   (word-lists :initform (make-instance 'word-lists))
   (files :reader forth-system-files :initform (make-instance 'files))
   (execution-tokens :initform (make-instance 'execution-tokens))
   (replacements :initform (make-instance 'replacements))
   (base :initform 10)
   (float-precision :initform 17)
   state
   (definition :initform nil)
   (compiling-paused? :initform nil)
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
    (add-space memory (files-saved-buffer-space files))
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
  `(with-slots (memory data-stack return-stack control-flow-stack exception-stack loop-stack float-stack definitions-stack
                word-lists files execution-tokens replacements base float-precision state definition compiling-paused?
                show-redefinition-warnings? reset-redefinition-warnings? show-definition-code?)
       ,fs
     (declare (ignorable memory data-stack return-stack control-flow-stack exception-stack loop-stack float-stack
                         definitions-stack word-lists files execution-tokens replacements base float-precision state
                         definition compiling-paused? show-redefinition-warnings? reset-redefinition-warnings?
                         show-definition-code?))
     ,@body))

(defmacro define-forth-method (name (fs &rest args) &body body)
  `(defmethod ,name ((,fs forth-system) ,@args)
     (with-forth-system (,fs)
       ,@body)))

(define-forth-method reset-interpreter/compiler (fs)
  (stack-reset data-stack)
  (stack-reset return-stack)
  (stack-reset control-flow-stack)
  (stack-reset exception-stack)
  (stack-reset loop-stack)
  (stack-reset float-stack)
  (stack-reset definitions-stack)
  (reset-input files)
  (reset-pictured-buffer memory)
  (setf (state fs) :interpreting)
  (setf definition nil)
  (setf compiling-paused? nil)
  )

(define-forth-method forth-toplevel (fs &key evaluate)
  (reset-interpreter/compiler fs)
  (when evaluate
    (source-push files :evaluate evaluate))
  (let ((fatal?
          (catch 'bye
            (loop
              (restart-case
                  (handler-case
                      (interpreter/compiler fs)
                    (forth-exception (e)
                      (unless (member (forth-exception-key e) '(:abort :quit))
                        (when *exception-prefix*
                          (write-string *exception-prefix*))
                        (write-line (forth-exception-phrase e)))
                      (clear-input)
                      (reset-interpreter/compiler fs)
                      (when (and *exception-hook* (not (eq (forth-exception-key e) :quit)))
                        (funcall *exception-hook* fs))))
                (abort () :report (lambda (stream) (write-string "Return to FORTH toplevel" stream))
                  (reset-interpreter/compiler fs)))))))
    (if fatal?
        ;;---*** TODO: Need to return non-zero exit if standalone app
        ()
        (when *exit-hook*
          (funcall *exit-hook* fs)))))

(define-forth-method interpreter/compiler (fs &key (toplevel? t))
  (loop with first = t
    do (loop for empty = t then nil
             while (input-available-p files)
             as token = (word files #\Space)
             ;; If there's more than one whitespace character at the end of a line, WORD will return a null TOKEN
             when token
               do (multiple-value-bind (type value)
                      (let ((local (and (eq (state fs) :compiling)
                                        (find token (locals-locals (definition-locals definition))
                                              :test #'string-equal :key #'local-name)))
                            (word (lookup word-lists token)))
                        (cond (local
                               (values :local local))
                              (word
                               (values :word word))
                              (t
                               (interpret-number token base))))
                    (when (null type)
                      (forth-exception :undefined-word "~A is not defined" token))
                    (case (state fs)
                      (:interpreting
                       (case type
                         (:word
                          (cond ((word-compile-only? value)
                                 (forth-exception :compile-only-word))
                                (t
                                 (forth-call fs value *interpreter-psuedo-pc*))))
                         (:single
                          (stack-push data-stack value))
                         (:double
                          (stack-push-double data-stack value))
                         (:float
                          (stack-push float-stack value))))
                      (:compiling
                       (case type
                         (:local
                          (add-forms-to-definition fs `(stack-push data-stack ,(local-symbol value))))
                         (:word
                          (cond ((word-immediate? value)
                                 (forth-call fs value *interpreter-psuedo-pc*))
                                ((word-inlineable? value)
                                 (apply #'add-forms-to-definition fs (reverse (word-inline-forms value))))
                                (t
                                 (add-forms-to-definition fs `(forth-call fs ,value ,(next-psuedo-pc definition))))))
                         (:single
                          (add-forms-to-definition fs `(stack-push data-stack ,value)))
                         (:double
                          (add-forms-to-definition fs `(stack-push-double data-stack ,value)))
                         (:float
                          (add-forms-to-definition fs `(stack-push float-stack ,value)))))))
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
                (source-pop files)
                (when (and (eq (state fs) :interpreting) (terminal-input-p files))
                  (write-line "OK.")))))))

(defun forth-call (fs word psuedo-pc)
  (with-forth-system (fs)
    (stack-push return-stack psuedo-pc)
    (unwind-protect
         (progn
           (apply (word-code word) fs (word-parameters word))
           (when (word-does> word)
             (apply (word-code (word-does> word)) fs (word-parameters word))))
      (stack-pop return-stack))))

;;;

(define-forth-method begin-compilation (fs &optional name)
  (unless (eq (state fs) :interpreting)
    (forth-exception :recursive-compile))
  (setf definition (make-definition :word (make-word name nil :smudge? t) :exit-branch (make-branch-reference :exit)
                                    :>body-address (data-space-high-water-mark memory))
        compiling-paused? nil)
  (stack-reset definitions-stack)
  (setf (state fs) :compiling)
  (when name
    (add-word (word-lists-compilation-word-list word-lists) (definition-word definition)
              :silent (falsep show-redefinition-warnings?)))
  ;; :NONAME creates a word without a name and places its "execution token" on the data stack
  (register-execution-token execution-tokens (definition-word definition) (definition->body-address definition)))

;;; Used by words that create words (e.g., CREATE, CONSTANT, etc.) to add the word to compilation word list
;;; and register its execution token
(define-forth-method add-and-register-word (fs word &optional >body-address)
  ;; Ensure that IMMEDIATE and DOES> will find this word
  (let ((>body-address (or >body-address (data-space-high-water-mark memory))))
    (setf definition (make-definition :word word :>body-address >body-address :in-progress? nil))
    (add-word (word-lists-compilation-word-list word-lists) word :silent (falsep show-redefinition-warnings?))
    (register-execution-token execution-tokens word >body-address)))

(define-forth-method add-forms-to-definition (fs &rest forms)
  (case (locals-state (definition-locals definition))
    (:none
     (let ((word (definition-word definition)))
       (setf (word-inline-forms word) (append (reverse forms) (word-inline-forms word)))))
    (:in-progress
     (forth-exception :unterminated-locals-block))
    (:complete
     (let ((locals (definition-locals definition)))
       (setf (locals-forms locals) (append (reverse forms) (locals-forms locals)))))))

(defmacro add-to-definition (fs &body body)
  `(add-forms-to-definition ,fs ,@body))

(define-forth-method start-local-definitions (fs)
  (setf (locals-state (definition-locals definition)) :in-progress))

(define-forth-method add-local-definition (fs name &optional (initialize? t))
  (let ((locals (definition-locals definition)))
    (when (eq (locals-state locals) :complete)
      (forth-exception :multiple-local-blocks))
    (when (or (plusp (stack-depth control-flow-stack)) (plusp (stack-depth loop-stack)))
      (forth-exception :locals-in-control-flow))
    (when (= (length (locals-locals locals)) +maximum-locals+)
      (forth-exception :too-many-locals))
    (when (eq (locals-state locals) :none)
      (start-local-definitions fs))
    (let ((local (make-local :name name :symbol (intern (string-upcase name) *forth-words-package*) :initialize? initialize?)))
      (push local (locals-locals locals)))))

(define-forth-method end-local-definitions (fs)
  (setf (locals-state (definition-locals definition)) :complete))

(define-forth-method finish-compilation (fs)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (unless (zerop (stack-depth control-flow-stack))
    (forth-exception :control-mismatch))
  (flet ((finish-definition ()
           (let* ((word (definition-word definition))
                  (name (intern (if (word-name word) (string-upcase (word-name word)) (symbol-name (gensym "XT")))
                                *forth-words-package*))
                  (locals-block
                    (let ((locals (definition-locals definition)))
                      (case (locals-state locals)
                        (:none)
                        (:in-progress
                         (forth-exception :unterminated-locals-block))
                        (:complete
                         `((let (,@(loop for local in (reverse (locals-locals locals))
                                         collect `(,(local-symbol local)
                                                   ,(if (local-initialize? local)
                                                        `(stack-pop data-stack)
                                                        0))))
                             (declare (ignorable ,@(reverse (loop for local in (locals-locals locals)
                                                                  collect (local-symbol local)))))
                             (tagbody
                                ,@(reverse (locals-forms locals)))))))))
                  (thunk `(named-lambda ,name (fs &rest parameters)
                            (declare (ignorable parameters))
                            (with-forth-system (fs)
                              (tagbody
                                 ,@(reverse (word-inline-forms word))
                                 ,@locals-block
                                 ,(branch-reference-tag (definition-exit-branch definition)))))))
             (setf (word-code word) (compile nil (eval thunk)))
             ;; Keep the forms for subsequent inlining and also for SEE
             (when locals-block
               ;; Ensure subsequent inlining of this definition will include the locals block
               (push (car locals-block) (word-inline-forms (definition-word definition))))
             (when (not (zerop show-definition-code?))
               (show-definition fs word))
             (setf (word-smudge? word) nil
                   (definition-in-progress? definition) nil))))
    (finish-definition)
    (loop while (plusp (stack-depth definitions-stack))
          do (let ((does>-word (definition-word definition)))
               (setf definition (stack-pop definitions-stack))
               (add-forms-to-definition fs `(execute-does> fs ,does>-word))
               (finish-definition)))
    ;; Leave the new definition in DEFINITION for use by IMMEDIATE and DOES>
    (setf compiling-paused? nil)
    (when (shiftf reset-redefinition-warnings? nil)
      (setf show-redefinition-warnings? +true+))
    (setf (state fs) :interpreting)))

(define-forth-method postpone (fs word)
  (cond ((word-immediate? word)
         (add-forms-to-definition fs `(forth-call fs ,word ,(next-psuedo-pc definition))))
        (t
         (add-forms-to-definition fs
           `(case (state fs)
              (:interpreting
               (forth-call fs ,word ,*interpreter-psuedo-pc*))
              (:compiling
               ,(if (word-inlineable? word)
                    `(apply #'add-forms-to-definition fs (reverse (word-inline-forms ,word)))
                    `(add-forms-to-definition fs '(forth-call fs ,word ,(next-psuedo-pc definition))))))))
        ;;---*** NOTE: I don't know under what circumstances POSTPONE should produce this error.
        ;;(t
        ;; (forth-exception :invalid-postpone))
        ))

(define-forth-method compile-comma (fs xt)
  (when (definition-in-progress? definition)
    (add-to-definition fs
      `(execute execution-tokens ,xt fs))))

(define-forth-method compile-does> (fs)
  (let ((does>-word (make-word (symbol-name (gensym "DOES>")) nil)))
    (stack-push definitions-stack definition)
    (setf definition (make-definition :word does>-word :exit-branch (make-branch-reference :exit)
                                      :>body-address (data-space-high-water-mark memory)))))

(define-forth-method execute-does> (fs does>-word)
  (unless definition
    (forth-exception :invalid-does>))
  (unless (word-creating-word? (definition-word definition))
    (forth-exception :invalid-does>))
  (setf (word-does> (definition-word definition)) does>-word))

(defun execute-compile-token (fs &rest parameters)
  (with-forth-system (fs)
    (stack-pop data-stack)
    (let ((word (first parameters)))
      (if (word-immediate? word)
          (forth-call fs word *interpreter-psuedo-pc*)
          (when (definition-in-progress? definition)
            (if (word-inlineable? word)
                (apply #'add-forms-to-definition fs (reverse (word-inline-forms word)))
                (add-forms-to-definition fs `(forth-call fs ,word ,(next-psuedo-pc definition)))))))))

(define-forth-method create-compile-execution-token (fs word)
  (if (word-compile-token word)
      (values 0 (word-compile-token word))
      (let* ((cword (make-word nil #'execute-compile-token :parameters (list word)))
             (cxt (register-execution-token execution-tokens cword (data-space-high-water-mark memory))))
        (setf (word-compile-token word) cxt)
        (values 0 cxt))))

(define-forth-method show-definition (fs word)
  (cond ((word-inline-forms word)
         (let ((thunk `(defun ,(intern (string-upcase (word-name word)) *forth-words-package*) (fs &rest parameters)
                         (declare (ignorable parameters))
                         (with-forth-system (fs)
                           (tagbody
                              ,@(reverse (word-inline-forms word))
                            :exit)))))
           (format t "~&Source code for ~A:" (word-name word))
           (let ((*package* (find-package '#:forth)))
             (pprint thunk)
             (terpri))))
        ((word-code word)
         (format t "~&Object code for ~A:~%" (word-name word))
         (disassemble (word-code word)))
        (t
         (format t "~&No human-readable definition of ~A~%" (word-name word)))))

;;;

(define-forth-method verify-control-structure (fs type &optional (n 1))
  type n ;; (declare (ignore type n))
  (when (zerop (stack-depth control-flow-stack))
    (forth-exception :control-mismatch))
    ;; Forth allows you to mix and match control structures. For example,
    ;;   : GI5 BEGIN DUP 2 > WHILE DUP 5 < WHILE DUP 1+ REPEAT 123 ELSE 345 THEN ;
    ;;   3 GI5 => 3 4 5 123
    #+ignore
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
      (add-forms-to-definition fs `(when ,condition
                                     (go ,(branch-reference-tag branch))))
      (add-forms-to-definition fs `(go ,(branch-reference-tag branch))))
  nil)

(defmacro execute-branch-when (fs branch &body body)
  (if (= (length body) 1)
      `(execute-branch ,fs ,branch ',@body)
      `(execute-branch ,fs ,branch '(progn ,@body))))

(define-forth-method resolve-branch (fs branch)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (add-forms-to-definition fs (branch-reference-tag branch))
  nil)

;;;

(defstruct (exception-frame (:constructor %make-exception-frame))
  data-stack-depth
  return-stack-depth
  control-flow-stack-depth
  float-stack-depth
  input-state
  )

(define-forth-method make-exception-frame (fs)
  (%make-exception-frame :data-stack-depth (stack-depth data-stack)
                         :return-stack-depth (stack-depth return-stack)
                         :control-flow-stack-depth (stack-depth control-flow-stack)
                         :float-stack-depth (stack-depth float-stack)
                         :input-state (save-input files :for-catch? t)))

(define-forth-method apply-exception-frame (fs frame)
  (setf (stack-depth data-stack) (exception-frame-data-stack-depth frame)
        (stack-depth return-stack) (exception-frame-return-stack-depth frame)
        (stack-depth control-flow-stack) (exception-frame-control-flow-stack-depth frame)
        (stack-depth float-stack) (exception-frame-float-stack-depth frame))
  (restore-input files (exception-frame-input-state frame) :for-throw? t))

