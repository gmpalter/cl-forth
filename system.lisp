(in-package #:forth)

(defstruct definition
  word
  exit-branch
  >body-address
  (in-progress? t))

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
   (definitions-stack :initform (make-instance 'stack :name "Definitions"
                                                      :initial-size 32
                                                      :underflow-key :definitions-stack-underflow
                                                      :overflow-key :definitions-stack-overflow))
   (word-lists :initform (make-instance 'word-lists))
   (files :initform (make-instance 'files))
   (execution-tokens :initform (make-instance 'execution-tokens))
   (base :initform 10)
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
  `(with-slots (memory data-stack return-stack control-flow-stack float-stack definitions-stack
                word-lists files execution-tokens base state definition compiling-paused?
                show-redefinition-warnings? reset-redefinition-warnings? show-definition-code?)
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
  (stack-reset definitions-stack)
  (reset-input files)
  (reset-pictured-buffer memory)
  (setf (state fs) :interpreting)
  (setf definition nil)
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
                                 (setf (word-inline-forms (definition-word definition))
                                       (append (reverse (word-inline-forms value))
                                               (word-inline-forms (definition-word definition)))))
                                (t
                                 (push `(forth-call fs ,value) (word-inline-forms (definition-word definition))))))
                         (:single
                          (push `(stack-push data-stack ,value) (word-inline-forms (definition-word definition))))
                         (:double
                          (push `(stack-push-double data-stack ,value) (word-inline-forms (definition-word definition))))
                         (:float
                          (push `(stack-push float-stack ,value) (word-inline-forms (definition-word definition))))))))
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

(defun forth-call (fs word)
  (with-forth-system (fs)
    (let ((return-address (multiple-value-bind (fn pc) (ccl::cfp-lfun (ccl::%get-frame-ptr))
                            (declare (ignore fn))
                            (cons word pc))))
      (stack-push return-stack return-address)
      (unwind-protect
           (progn
             (apply (word-code word) fs (word-parameters word))
             (when (word-does> word)
               (apply (word-code (word-does> word)) fs (word-parameters word))))
        (stack-pop return-stack)))))

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

(defmacro add-to-definition (fs &body body)
  (declare (ignore fs))
  `(progn
     ,@(loop for form in body
             collect `(push ,form (word-inline-forms (definition-word definition))))))

(define-forth-method finish-compilation (fs)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (unless (zerop (stack-depth control-flow-stack))
    (forth-exception :control-mismatch))
  (flet ((finish-definition ()
           (let ((thunk `(lambda (fs &rest parameters)
                           (declare (ignorable parameters))
                           (with-forth-system (fs)
                             (tagbody
                                ,@(reverse (word-inline-forms (definition-word definition)))
                                ,(branch-reference-tag (definition-exit-branch definition)))))))
             (when (not (zerop show-definition-code?))
               (format t "~&Code for ~A:~%  ~:W~%" (or (word-name (definition-word definition)) "<execution token>") thunk))
             (setf (word-code (definition-word definition)) (compile nil thunk)))
           ;; Keep the forms for SHOW-DEFINITION
           ;;(setf (word-inline-forms (definition-word definition)) nil)
           (setf (word-smudge? (definition-word definition)) nil
                 (definition-in-progress? definition) nil)))
    (finish-definition)
    (loop while (plusp (stack-depth definitions-stack))
          do (let ((does>-word (definition-word definition)))
               (setf definition (stack-pop definitions-stack))
               (push `(execute-does> fs ,does>-word) (word-inline-forms (definition-word definition)))
               (finish-definition)))
    ;; Leave the new definition in DEFINITION for use by IMMEDIATE and DOES>
    (setf compiling-paused? nil)
    (when (shiftf reset-redefinition-warnings? nil)
      (setf show-redefinition-warnings? +true+))
    (setf (state fs) :interpreting)))

(define-forth-method postpone (fs word)
  (cond ((word-immediate? word)
         (push `(forth-call fs ,word) (word-inline-forms (definition-word definition))))
        (t
         (push `(case (state fs)
                  (:interpreting
                   (forth-call fs ,word))
                  (:compiling
                   ,(if (word-inlineable? word)
                        `(setf (word-inline-forms (definition-word definition))
                               (append (reverse (word-inline-forms ,word))
                                       (word-inline-forms (definition-word definition))))
                        `(push '(forth-call fs ,word) (word-inline-forms (definition-word definition))))))
               (word-inline-forms (definition-word definition))))
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
    )
  (unless (word-creating-word? (definition-word definition))
    )
  (setf (word-does> (definition-word definition)) does>-word))

(define-forth-method show-definition (fs word)
  (cond ((word-inline-forms word)
         (let ((thunk `(defun ,(make-symbol (string-upcase (word-name word))) (fs &rest parameters)
                         (declare (ignorable parameters))
                         (with-forth-system (fs)
                           (tagbody
                              ,@(reverse (word-inline-forms word))
                            :exit)))))
           (format t "~&Source code:")
           (let ((*package* (find-package '#:forth)))
             (pprint thunk)
             (terpri))))
        ((word-code word)
         (format t "~&Object code for ~A:~%" (word-name word))
         (disassemble (word-code word)))
        (t
         (format t "~&No human-readable definition of ~A~%" (word-name word)))))

;;;

(defstruct (branch-reference (:constructor %make-branch-reference))
  type
  tag)

(declaim (inline make-branch-reference))
(defun make-branch-reference (type)
  (%make-branch-reference :type type :tag (gensym (symbol-name type))))

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
      (push `(when ,condition
               (go ,(branch-reference-tag branch)))
            (word-inline-forms (definition-word definition)))
      (push `(go ,(branch-reference-tag branch)) (word-inline-forms (definition-word definition))))
  nil)

(defmacro execute-branch-when (fs branch &body body)
  (if (= (length body) 1)
      `(execute-branch ,fs ,branch ',@body)
      `(execute-branch ,fs ,branch '(progn ,@body))))

(define-forth-method resolve-branch (fs branch)
  (unless (eq (state fs) :compiling)
    (forth-exception :not-compiling))
  (push (branch-reference-tag branch) (word-inline-forms (definition-word definition)))
  nil)

