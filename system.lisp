;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:forth)

(defparameter *forth-package* (find-package '#:forth))

(defconstant +maximum-locals+ 32)

(deftype forth-boolean () `(member ,+false+ ,+true+))

(defstruct local
  name
  symbol
  initialize?)

(defstruct locals
  state
  locals
  forms)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct definition
  word
  (locals (make-locals :state :none))
  exit-branch
  >body-address
  (call-site 0)
  (in-progress? t))
)

(defstruct (branch-reference (:constructor %make-branch-reference))
  type
  tag)

(declaim (inline make-branch-reference))
(defun make-branch-reference (type)
  (%make-branch-reference :type type :tag (gensym (symbol-name type))))

(declaim (inline next-psuedo-pc))
(defun next-psuedo-pc (definition)
  (make-psuedo-pc (definition-word definition) (incf (definition-call-site definition))))

(defstruct (forth-system (:conc-name fs-) (:constructor %make-forth-system) (:print-function %print-forth-system))
  (memory (make-memory) :type memory :read-only t)
  (data-stack (make-stack "Data" 1024 :stack-overflow :stack-underflow) :type stack :read-only t)
  (return-stack (make-stack "Return" 128 :return-stack-overflow :return-stack-underflow) :type stack :read-only t)
  (control-flow-stack (make-stack "Control-flow" 128 :control-flow-stack-overflow :control-flow-stack-underflow) :type stack :read-only t)
  (exception-stack (make-stack "Exception" 128 :exception-stack-overflow :exception-stack-underflow) :type stack :read-only t)
  (loop-stack (make-stack "Loop Control" 32 :do-loops-nesting :loop-stack-underflow) :type stack :read-only t)
  (float-stack (make-stack "Floating-point" 32 :float-stack-overflow :float-stack-underflow) :type stack :read-only t)
  (definitions-stack (make-stack "Definitions" 32 :definitions-stack-overflow :definitions-stack-underflow) :type stack :read-only t)
  (word-lists (make-word-lists) :type word-lists :read-only t)
  (files (make-instance 'files) :type files :read-only t)
  (execution-tokens (make-instance 'execution-tokens) :type execution-tokens :read-only t)
  (ffi (make-instance 'ffi) :type ffi :read-only t)
  (replacements (make-instance 'replacements) :type replacements :read-only t)
  (base 10 :type fixnum)
  (float-precision 17 :type fixnum)
  (%state 0 :type fixnum)
  (definition nil :type (or definition null))
  (compiling-paused? nil :type boolean)
  (show-redefinition-warnings? +true+ :type forth-boolean)
  (reset-redefinition-warnings? nil :type boolean)
  (show-definition-code? +false+ :type forth-boolean)
  (%optimize-definitions? +false+ :type forth-boolean)
  (show-backtraces-on-error? +false+ :type forth-boolean)
  (current-frame nil)
  ;; If non-NIL, called after an exception, including ABORT and ABORT\", to perform additional processing
  (exception-hook  nil)
  ;; If non-NIL, display this string before displaying the exception's phrase
  (exception-prefix nil :type (or string null))
  ;; If non-NIL, called before a non-fatal exit to perform additional processing
  (exit-hook nil)
  ;; If non-NIL, a string that's displayed after Forth's initial announcement
  (announce-addendum nil :type (or string null))
  (prompt-string #.(format nil "OK.~%") :type string)
  (extensions nil))

(defun %print-forth-system (fs stream depth)
  (declare (ignore depth))
  (print-unreadable-object (fs stream :type t :identity t)))

(defmacro with-forth-system ((fs) &body body)
  `(symbol-macrolet (,@(map 'list #'(lambda (slot accessor)
                                      `(,slot (,accessor ,fs)))
                            '(memory data-stack return-stack control-flow-stack exception-stack loop-stack
                              float-stack definitions-stack word-lists files execution-tokens ffi
                              replacements base float-precision %state definition compiling-paused?
                              show-redefinition-warnings? reset-redefinition-warnings? show-definition-code?
                              %optimize-definitions?
                              show-backtraces-on-error? current-frame exception-hook exception-prefix exit-hook
                              announce-addendum prompt-string extensions)
                            '(fs-memory fs-data-stack fs-return-stack fs-control-flow-stack fs-exception-stack fs-loop-stack
                              fs-float-stack fs-definitions-stack fs-word-lists fs-files fs-execution-tokens fs-ffi
                              fs-replacements fs-base fs-float-precision fs-%state fs-definition fs-compiling-paused?
                              fs-show-redefinition-warnings? fs-reset-redefinition-warnings? fs-show-definition-code?
                              fs-%optimize-definitions?
                              fs-show-backtraces-on-error? fs-current-frame fs-exception-hook fs-exception-prefix fs-exit-hook
                              fs-announce-addendum fs-prompt-string fs-extensions)))
     #+LispWorks
     (declare (ignorable memory data-stack return-stack control-flow-stack exception-stack loop-stack
                         float-stack definitions-stack word-lists files execution-tokens ffi
                         replacements base float-precision %state definition compiling-paused?
                         show-redefinition-warnings? reset-redefinition-warnings? show-definition-code? optimize-definitions?
                         show-backtraces-on-error? current-frame exception-hook exception-prefix exit-hook
                         announce-addendum prompt-string extensions))
     ,@body))

(defmacro define-forth-function (name (fs &rest args) &body body)
  (multiple-value-bind (body declarations doc)
      (uiop:parse-body body)
    (declare (ignore doc))
    `(defun ,name (,fs ,@args)
       (declare (optimize (speed 3) (safety 0))
                (type forth-system ,fs))
       ,@declarations
       (with-forth-system (,fs)
         ,@body))))

(defun make-forth-system (&key template)
  (let ((fs (%make-forth-system)))
    (declare (type forth-system fs) (optimize (speed 3) (safety 0)))
    (with-forth-system (fs)
      (add-state-space memory fs)
      (add-state-space memory word-lists)
      (add-state-space memory files)
      (add-space memory (files-source-as-space files))
      (add-space memory (files-saved-buffer-space files))
      (add-space memory execution-tokens)
      (add-space memory (ffi-foreign-space ffi))
      (cond (template
             (load-from-template fs template fs))
            (t
             (reset-word-lists word-lists)
             (register-predefined-words word-lists execution-tokens (data-space-high-water-mark memory)))))
    fs))

(defun state (fs)
  (declare (type forth-system fs) (optimize (speed 3) (safety 0)))
  (if (zerop (fs-%state fs))
      :interpreting
      :compiling))

(defun (setf state) (value fs)
  (declare (type forth-system fs) (optimize (speed 3) (safety 0)))
  (setf (fs-%state fs) (ecase value
                         (:interpreting 0)
                         (:compiling 1)))
  value)

(defun optimize-definitions? (fs)
  (declare (type forth-system fs) (optimize (speed 3) (safety 0)))
  (truep (fs-%optimize-definitions? fs)))

(defun (setf optimize-definitions?) (value fs)
  (declare (type forth-system fs) (type boolean value) (optimize (speed 3) (safety 0)))
  (setf (fs-%optimize-definitions? fs) (if value +true+ +false+))
  value)

(define-forth-function reset-interpreter/compiler (fs)
  (stack-reset data-stack)
  (stack-reset return-stack)
  (setf current-frame nil)
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

(define-forth-function forth-toplevel (fs &key interpret)
  (reset-interpreter/compiler fs)
  (when interpret
    (etypecase interpret
      (string
       (source-push files :evaluate interpret))
      (list
       ;; Lines to be interpreted are pushed onto the list, leaving the list in reverse order of execution.
       ;; By pushing the lines onto the source stack, we reverse that reversal so the lines are interpreted in the proper order.
       (dolist (line interpret)
         (source-push files :evaluate line)))))
  (let ((fatal?
          (catch 'bye
            (loop
              (restart-case
                  (handler-case
                      (interpreter/compiler fs)
                    (forth-exception (e)
                      (unless (member (forth-exception-key e) '(:abort :quit))
                        (when exception-prefix
                          (write-string exception-prefix))
                        (write-line (forth-exception-phrase e)))
                      (when (truep show-backtraces-on-error?)
                        (show-backtrace fs))
                      (clear-input)
                      (reset-interpreter/compiler fs)
                      (when (and exception-hook (not (eq (forth-exception-key e) :quit)))
                        (funcall exception-hook fs))
                      (force-output))
                    ;; Intercept Ctrl-C and return to top level
                    #+(or CCL SBCL)
                    (#+CCL ccl:interrupt-signal-condition #+CCL ()
                     #+SBCL sb-sys:interactive-interrupt #+SBCL ()
                      (when exception-prefix
                        (write-string exception-prefix))
                      (write-line "Forced ABORT")
                      (when (truep show-backtraces-on-error?)
                        (show-backtrace fs))
                      (clear-input)
                      (reset-interpreter/compiler fs)
                      (when exception-hook
                        (funcall exception-hook fs))
                      (force-output)))
                (abort () :report (lambda (stream) (write-string "Return to FORTH toplevel" stream))
                  (reset-interpreter/compiler fs)))))))
    (ignore-errors
     ;; Suppress any errors if *standard-output* is closed prematurely
     (report-statistics fs))
    ;; Return T if the interpreter loop exited cleanly. Return NIL on a fatal error, usually detected by an exception hook
    (if fatal?
        nil
        (prog1
            t
          (when exit-hook
            (funcall exit-hook fs))))))

(define-forth-function report-statistics (fs)
  (let ((words-created (word-lists-words-created word-lists))
        (object-code-size (word-lists-object-code-size word-lists)))
    (multiple-value-bind (memory-allocated memory-preallocated)
        (memory-usage memory)
      (let ((memory-allocated (- memory-allocated memory-preallocated)))
        (when (plusp (+ words-created memory-allocated object-code-size))
          (write-line "In this session:")
          (when (plusp words-created)
            (format t "  ~D definition~:P created~%" words-created))
          (when (plusp object-code-size)
            (format t "  ~D byte~:P of object code generated~%" object-code-size))
          (when (plusp memory-allocated)
            (format t "  ~D byte~:P of memory allocated~@[, ~D byte~:P preallocated~]~%"
                    memory-allocated (and (plusp memory-preallocated) memory-preallocated)))
          (force-output)
          t)))))

;;; CCL doesn't signal a condition when the user presses Ctrl-C. But, it does invoke its break loop
;;; with a specific condition (INTERRUPT-SIGNAL-CONDITION). If the break loop is being entered for
;;; that condition, signal it first to allow CL-Forth to catch it an abort to its top level.
#+CCL
(ccl:advise ccl::cbreak-loop ;; (msg cont-string condition *top-error-frame*)
            (when (typep (third ccl:arglist) 'ccl:interrupt-signal-condition)
              (signal (third ccl:arglist)))
            :when :before
            :name signal-interrupt-signal-condition)

(define-forth-function interpreter/compiler (fs &key (toplevel? t))
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
                                 ;; See REWRITE-TAGS, below, for an explanation
                                 (multiple-value-bind (forms exit-tag)
                                     (rewrite-tags (word-inline-forms value))
                                   (apply #'add-forms-to-definition fs (reverse (update-forth-calls forms definition)))
                                   (when exit-tag
                                     (add-forms-to-definition fs exit-tag))))
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
                  (write-string prompt-string)
                  (force-output)))
       (unless (refill files)
         (cond ((not toplevel?)
                (source-pop files)
                (return-from interpreter/compiler nil))
               ((terminal-input-p files)
                (throw 'bye nil))
               (t
                (source-pop files)
                (when (and (eq (state fs) :interpreting) (terminal-input-p files))
                  (write-string prompt-string)
                  (force-output)))))))

(defun forth-call (fs word psuedo-pc)
  (declare (type forth-system fs) (type word word)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push return-stack psuedo-pc)
    (setf current-frame (make-psuedo-pc word -1))
    (funcall (word-code word) fs (word-parameters word))
    (when (word-does> word)
      (funcall (word-code (word-does> word)) fs (word-parameters word)))
    (setf current-frame (stack-pop return-stack))))

(define-forth-function show-backtrace (fs)
  (let* ((cells (stack-cells return-stack))
         (depth (stack-depth return-stack))
         (nframes (reduce #'(lambda (n cell) (if (psuedo-pc-p cell) (1+ n) n)) cells :initial-value 0)))
    (if (and (null current-frame) (zerop nframes))
        (write-line "Backtrace unavailable?")
        (let ((frame -1))
          (write-line "Backtrace:")
          (when current-frame
            (format t "~2D: ~A~%" (incf frame) (or (word-name (ppc-word current-frame)) "<Anonymous>")))
          (dotimes (i depth)
            (let ((cell (aref cells (- depth i 1))))
              (when (psuedo-pc-p cell)
                (format t "~2D: ~A~%" (incf frame) cell)))))))
  (show-stack data-stack base))

;;;

(define-forth-function begin-compilation (fs &optional name)
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
(define-forth-function add-and-register-word (fs word &optional >body-address)
  ;; Ensure that IMMEDIATE and DOES> will find this word
  (let ((>body-address (or >body-address (data-space-high-water-mark memory))))
    (setf definition (make-definition :word word :>body-address >body-address :in-progress? nil))
    (add-word (word-lists-compilation-word-list word-lists) word :silent (falsep show-redefinition-warnings?))
    (register-execution-token execution-tokens word >body-address)))

(define-forth-function add-forms-to-definition (fs &rest forms)
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

(define-forth-function start-local-definitions (fs)
  (setf (locals-state (definition-locals definition)) :in-progress))

(define-forth-function add-local-definition (fs name &optional (initialize? t))
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

(define-forth-function end-local-definitions (fs)
  (setf (locals-state (definition-locals definition)) :complete))

(define-forth-function finish-compilation (fs)
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
                         `(;; (flush-optimizer-stack)
                           (let (,@(loop for local in (reverse (locals-locals locals))
                                         collect `(,(local-symbol local)
                                                   ,(if (local-initialize? local)
                                                        `(stack-pop data-stack)
                                                        0))))
                             (declare (ignorable ,@(reverse (loop for local in (locals-locals locals)
                                                                  collect (local-symbol local)))))
                             (tagbody
                                ,@(reverse (locals-forms locals)))))))))
                  (body `(,@(reverse (word-inline-forms word))
                          ,@locals-block))
                  (optimized-body (if (optimize-definitions? fs)
                                      (optimize-definition body)
                                      body))
                  (thunk `(named-lambda ,name (fs parameters)
                            (declare (type forth-system fs) (type parameters parameters) (ignorable fs parameters)
                                     (optimize (speed 3) (safety 0)))
                            (with-forth-system (fs)
                              (tagbody
                                 ,@optimized-body
                                 ,(branch-reference-tag (definition-exit-branch definition)))))))
             (setf (word-code word) (compile nil (eval thunk)))
             (note-object-code-size word-lists word)
             ;; Keep the original forms for inlining to allow additional optimizations
             ;; and keep the optimized forms for SEE
             (setf (word-inline-forms (definition-word definition)) (reverse body)
                   (word-optimized-forms (definition-word definition)) (or (reverse optimized-body) '(nil)))
             (when (truep show-definition-code?)
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

(define-forth-function postpone (fs word)
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

(define-forth-function compile-comma (fs xt)
  (when (definition-in-progress? definition)
    (add-to-definition fs
      `(execute execution-tokens ,xt fs))))

(define-forth-function compile-does> (fs)
  (let ((does>-word (make-word (symbol-name (gensym "DOES>")) nil)))
    (stack-push definitions-stack definition)
    (setf definition (make-definition :word does>-word :exit-branch (make-branch-reference :exit)
                                      :>body-address (data-space-high-water-mark memory)))))

(define-forth-function execute-does> (fs does>-word)
  (unless definition
    (forth-exception :invalid-does>))
  (let ((word (definition-word definition)))
    (unless (word-created-word? word)
      (forth-exception :invalid-does>))
    ;; There is no way to declare a DOES> word inlineable. So, if the word being defined might be inlineable (i.e., has
    ;; inline forms), add the DOES> word's inline forms to the word being defined.
    (when (word-inline-forms word)
      (if (word-inline-forms does>-word)
          (progn
            (apply #'add-forms-to-definition fs (reverse (word-inline-forms does>-word)))
            (when (optimize-definitions? fs)
              (setf (word-optimized-forms word) (reverse (optimize-definition (reverse (word-inline-forms word)))))))
          (add-forms-to-definition fs `(funcall (word-code ,does>-word) fs ,(word-parameters word)))))
    (setf (word-does> word) does>-word)))

;;; When a word is defined as inlineable, its forms will be inserted into any calling words at the call site.
;;; If the inlined word uses any flow control constructs (e.g., IF/THEN/ELSE), the word's forms will include tags.
;;; In this case, we must rewrite the tags to avoid ending up with duplicate tags in the calling word if the
;;; inlined word is called more than once in the calling word.
;;; Additionally, if the inlined word uses EXIT, we'll need to add an unique EXIT label after the word's forms so that
;;; if the inlined word executes the EXIT, it will transfer control to the calling word immediately after the call site.
;;; Note: Rewriting tags also applies to any word defined by a creating word with DOES> as the DOES> forms will
;;; be included in the forms of the created word.
(defun rewrite-tags (forms)
  (let ((tags (loop for form in forms
                    when (atom form)
                      collect form))
        (exit
          (labels ((exit? (tag)
                     (let ((tag-name (symbol-name tag)))
                       (equal (subseq tag-name 0 (position-if #'digit-char-p tag-name)) "EXIT")))
                   (goto-exit? (form)
                     (cond ((atom form) nil)
                           ((and (eq (first form) 'go) (exit? (second form)))
                            (second form))
                           ((or (goto-exit? (car form)) (goto-exit? (cdr form)))))))
            (loop for form in forms
                  thereis (goto-exit? form)))))
    (if (or tags exit)
        (let ((substitutions (loop for tag in tags
                                   for tag-name = (symbol-name tag)
                                   collect `(,tag
                                             . ,(gensym (subseq tag-name 0 (position-if #'digit-char-p tag-name)))))))
          (if (and exit (null (assoc exit substitutions)))
              (push `(,exit . ,(gensym "EXIT")) substitutions)
              (setf exit nil))
          (values (sublis substitutions forms) (cdr (assoc exit substitutions))))
        forms)))

;;; When inlining a word into an outer word, we must update any FORTH-CALLs in the inner word
;;; to use PSUEDO-PCs that reference the outer word. Otherwise, backtraces will make little sense.
(defun update-forth-calls (forms definition)
  (labels ((forth-call? (form fixup?)
             (cond ((atom form) nil)
                   ((eq (first form) 'forth-call)
                    (when fixup?
                      (setf (fourth form) (next-psuedo-pc definition)))
                    t)
                   ((or (forth-call? (car form) fixup?) (forth-call? (cdr form) fixup?))))))
    (if (loop for form in forms
                thereis (forth-call? form nil))
        (let ((forms (copy-tree forms)))
          (loop for form in forms
                do (forth-call? form t))
          forms)
        forms)))

(defun execute-compile-token (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-pop data-stack)
    (let ((word (parameters-p1 parameters)))
      (if (word-immediate? word)
          (forth-call fs word *interpreter-psuedo-pc*)
          (when (definition-in-progress? definition)
            (if (word-inlineable? word)
                (apply #'add-forms-to-definition fs (reverse (word-inline-forms word)))
                (add-forms-to-definition fs `(forth-call fs ,word ,(next-psuedo-pc definition)))))))))

(define-forth-function create-compile-execution-token (fs word)
  (if (word-compile-token word)
      (values 0 (word-compile-token word))
      (let* ((cword (make-word nil #'execute-compile-token :parameters (make-parameters word)))
             (cxt (register-execution-token execution-tokens cword (data-space-high-water-mark memory))))
        (setf (word-compile-token word) cxt)
        (values 0 cxt))))

(define-forth-function show-definition (fs word)
  (declare (ignore fs))
  (flet ((show-documentation (add-newline?)
           (when (word-documentation word)
             (dolist (line (word-documentation word))
               (format t "~&;;; ~A" line))
             (when add-newline?
               (terpri)))))
    (cond ((or (word-optimized-forms word) (word-inline-forms word))
           (let ((thunk `(defun ,(intern (string-upcase (word-name word)) *forth-words-package*) (fs parameters)
                           (declare (ignorable parameters))
                           (with-forth-system (fs)
                             (tagbody
                                ,@(reverse (or (word-optimized-forms word) (word-inline-forms word)))
                              :exit)))))
             (format t "~&Source code for ~A:" (word-name word))
             (show-documentation nil)
             (let ((*package* *forth-package*)
                   (*print-right-margin* 95))
               (pprint thunk)
               (terpri))))
          ((word-code word)
           (format t "~&Object code for ~A:~%" (word-name word))
           (show-documentation t)
           (disassemble (word-code word)))
          (t
           (format t "~&No human-readable definition of ~A~%" (word-name word))))))

;;;

(define-forth-function verify-control-structure (fs type &optional (n 1))
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

(define-forth-function control-structure-push (fs branch)
  (stack-push control-flow-stack branch))

(define-forth-function control-structure-find (fs type &optional (n 0))
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

(define-forth-function control-structure-pop (fs type)
  "Find the most recent TYPE entry on the control stack, remove it from the stack, and return it"
  (let ((n (stack-find-if #'(lambda (cell) (eq (branch-reference-type cell) type)) control-flow-stack)))
    (if n
        (stack-snip control-flow-stack n)
        (forth-exception :control-mismatch))))

(define-forth-function execute-branch (fs branch &optional condition)
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

(define-forth-function resolve-branch (fs branch)
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

(define-forth-function make-exception-frame (fs)
  (%make-exception-frame :data-stack-depth (stack-depth data-stack)
                         :return-stack-depth (stack-depth return-stack)
                         :control-flow-stack-depth (stack-depth control-flow-stack)
                         :float-stack-depth (stack-depth float-stack)
                         :input-state (save-input files :for-catch? t)))

(define-forth-function apply-exception-frame (fs frame)
  (setf (stack-depth data-stack) (exception-frame-data-stack-depth frame)
        (stack-depth return-stack) (exception-frame-return-stack-depth frame)
        (stack-depth control-flow-stack) (exception-frame-control-flow-stack-depth frame)
        (stack-depth float-stack) (exception-frame-float-stack-depth frame))
  (restore-input files (exception-frame-input-state frame) :for-throw? t))

