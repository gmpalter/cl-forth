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

(defstruct optimizer
  (data-stack (make-stack "Optimizer" 1024 :optimizer-stack-overflow :optimizer-stack-underflow))
  (explicit-pops 0)
  (pushed-after-explicit-pops 0)
  (return-stack-depth 0)
  (pictured-buffer-active? :unknown))

(defun pop-optimizer-data-stack (optimizer)
  (cond ((plusp (optimizer-pushed-after-explicit-pops optimizer))
         (prog1
             (stack-pop (optimizer-data-stack optimizer))
           (decf (optimizer-pushed-after-explicit-pops optimizer))))
        ((plusp (optimizer-explicit-pops optimizer))
         (prog1
             `(stack-pop data-stack)
           (decf (optimizer-explicit-pops optimizer))))
        (t
         (stack-pop (optimizer-data-stack optimizer)))))
  
(defun empty-optimizer-data-stack (optimizer &key contains depth conditional?)
  (let* ((optimizer-stack (optimizer-data-stack optimizer))
         (stack-depth (stack-depth optimizer-stack))
         (saved-state (when conditional?
                        (list (stack-contents (optimizer-data-stack optimizer))
                              (optimizer-explicit-pops optimizer)
                              (optimizer-pushed-after-explicit-pops optimizer)))))
    (labels ((references? (expr)
               (cond ((member expr contains :test #'equal))
                     ((atom expr) nil)
                     (t
                      (or (references? (car expr)) (references? (cdr expr))))))
             (stack-pop? (expr)
               (cond ((atom expr) nil)
                     ((equal expr '(stack-pop data-stack)))
                     (t
                      (or (stack-pop? (car expr)) (stack-pop? (cdr expr))))))
             (clear-counters ()
               (setf (optimizer-explicit-pops optimizer) 0
                     (optimizer-pushed-after-explicit-pops optimizer) 0))
             (empty-stack (count)
               (reverse (loop for i below count
                              collect `(stack-push data-stack ,(stack-pop optimizer-stack)))))
             (reset ()
               (when conditional?
                 (setf (stack-contents (optimizer-data-stack optimizer)) (first saved-state)
                       (optimizer-explicit-pops optimizer) (second saved-state)
                       (optimizer-pushed-after-explicit-pops optimizer) (third saved-state)))))
      (cond ((zerop stack-depth)
             (clear-counters)
             nil)
            ((plusp (optimizer-explicit-pops optimizer))
             (let* ((pops (optimizer-explicit-pops optimizer))
                    (extras (optimizer-pushed-after-explicit-pops optimizer))
                    (extras-forms (when (plusp extras)
                                    (prog1
                                        (empty-stack extras)
                                      (decf stack-depth extras)))))
               (clear-counters)
               (prog1
                   (append (loop for i below stack-depth
                                 for cell = (stack-cell optimizer-stack i)
                                 collect `(stack-push data-stack ,cell)
                                 collect `(stack-roll-down data-stack ,(+ pops i)))
                           extras-forms)
                 (setf (stack-depth optimizer-stack) 0)
                 (reset))))
            (contains
             (clear-counters)
             ;; Find the deepest entry that references one of the contained forms and
             ;; pop it and everything after it from the stack.
             (let ((count (loop for i from (if depth (- stack-depth depth) 0) below stack-depth
                                for cell = (stack-cell optimizer-stack (- stack-depth i 1))
                                when (references? cell)
                                  return (- stack-depth i)
                                finally (return 0))))
               (when (plusp count)
                 ;; However, if anything below that deepest reference involves popping the stack,
                 ;; we have to also pop all the entries between that stack popper and the deepest
                 ;; reference to ensure the stack is consistent with what the definition expects.
                 ;; So, we'll look for the deepest stack popper to be certain.
                 (loop for i below (- stack-depth count)
                       for cell = (stack-cell optimizer-stack (- stack-depth i 1))
                       when (stack-pop? cell)
                         do (setf count (- stack-depth i))
                            (loop-finish))
                 (prog1
                     (empty-stack count)
                   (setf (optimizer-explicit-pops optimizer) count)
                   (reset)))))
            ((and depth (< depth stack-depth))
             (clear-counters)
             ;; Flush a specific number of forms off the stack.
             (prog1
                 (empty-stack depth)
               (setf (optimizer-explicit-pops optimizer) depth)
               (reset)))
            (t
             (clear-counters)
             ;; Caller wants the entire optimizer stack emptied
             (prog1
                 (empty-stack stack-depth)
               (reset)))))))


;;; Expression optimization

(defparameter +unknown-value+ '#:unknown)

(defun optimize-expr (optimizer topexpr bindings)
  (let ((substituted? nil))
    (labels ((optimize (expr toplevel?)
               (declare (ignore toplevel?))
               (cond ((atom expr)
                      (let ((binding (assoc expr bindings)))
                        (cond ((null binding)
                               expr)
                              ((numberp (second binding))
                               (second binding))
                              ((and (constantp (second binding)) (symbolp (second binding)))
                               (symbol-value (second binding)))
                              (t
                               expr))))
                     ((equal expr '(stack-pop data-stack))
                      (cond ((plusp (optimizer-explicit-pops optimizer))
                             (throw 'empty-and-retry t))
                            ((zerop (stack-depth (optimizer-data-stack optimizer)))
                             (if substituted?
                                 expr
                                 (return-from optimize-expr topexpr)))
                            (t
                             (prog1
                                 (pop-optimizer-data-stack optimizer)
                               (setf substituted? t)))))
                     ((member (first expr) '(stack-pop-double stack-pop-double-unsigned))
                      (let ((function (first expr)))
                        (cond ((plusp (optimizer-explicit-pops optimizer))
                               (throw 'empty-and-retry t))
                              ((>= (stack-depth (optimizer-data-stack optimizer)) 2)
                               (prog1
                                   (if (and (numberp (stack-cell (optimizer-data-stack optimizer) 0))
                                            (numberp (stack-cell (optimizer-data-stack optimizer) 1)))
                                       (case function
                                         (stack-pop-double
                                          (stack-pop-double (optimizer-data-stack optimizer)))
                                         (stack-pop-double-unsigned
                                          (stack-pop-double-unsigned (optimizer-data-stack optimizer))))
                                       (let ((high (pop-optimizer-data-stack optimizer))
                                             (low (pop-optimizer-data-stack optimizer)))
                                         `(progn
                                            (stack-push data-stack ,low)
                                            (stack-push data-stack ,high)
                                            ,expr)))
                                 (setf substituted? t)))
                              (t
                               (if substituted?
                                   expr
                                   (return-from optimize-expr topexpr))))))
                     ((equal expr '(stack-pop return-stack))
                      (prog1
                          expr
                        (when (plusp (optimizer-return-stack-depth optimizer))
                          (decf (optimizer-return-stack-depth optimizer)))))
                     ((and (eq (first expr) 'stack-cell) (eq (second expr) 'data-stack) (numberp (third expr))
                           (< (third expr) (stack-depth (optimizer-data-stack optimizer)))
                           ;; Can't perform this optimization if the top of the optimizer
                           ;; stack isn't in sync with the top of the data stack
                           (or (zerop (optimizer-explicit-pops optimizer))
                               (plusp (optimizer-pushed-after-explicit-pops optimizer))))
                      (stack-cell (optimizer-data-stack optimizer) (third expr)))
                     ;; If the expression passed to CELL-SIGNED, etc. is a constant, we can eliminate the call here.
                     ;; Also, eliminate the extra call when we run into (CELL-SIGNED (CELL-SIGNED ...)), etc.
                     ((member (first expr) '(cell-signed cell-unsigned
                                             double-cell-signed double-cell-unsigned
                                             quad-byte-signed quad-byte-unsigned
                                             double-byte-signed double-byte-unsigned
                                             extract-char native-char))
                      (let ((function (first expr))
                            (value (optimize (second expr) nil)))
                        (cond ((numberp value)
                               (funcall function value))
                              ;; Catch references to constants like +FORTH-CHAR-SPACE+
                              ((and (constantp value) (symbolp value))
                               (funcall function (symbol-value value)))
                              ((and (listp value) (eq (first value) function))
                               value)
                              (t
                               `(,function ,value)))))
                     (t
                      (loop for subexpr in expr
                            collect (optimize subexpr nil))))))
      (optimize topexpr t))))


;;; Optimizers

(defvar *optimizers* (make-hash-table))

(defmacro define-optimizer (name (optimizer form bindings) &body body)
  (multiple-value-bind (body declarations doc)
      (uiop:parse-body body)
    (declare (ignore doc))
    (flet ((define (name)
             (let ((optimizer-fun (intern (format nil "OPTIMIZE-~A" name))))
               `(setf (gethash ',name *optimizers*)
                      (named-lambda ,optimizer-fun (,optimizer ,form ,bindings)
                        ,@declarations
                        (macrolet ((punt (&optional (form 'form))
                                     `(append (empty-optimizer-data-stack optimizer) (list ,form)))
                                   (punt-if-data-stack-pop (expr)
                                     `(labels ((pop? (expr)
                                                 (cond ((atom expr)
                                                        nil)
                                                       ((equal expr '(stack-pop data-stack)))
                                                       (t
                                                        (or (pop? (car expr)) (pop? (cdr expr)))))))
                                        (if (pop? ,expr)
                                            (append (empty-optimizer-data-stack optimizer) (list form))
                                            (list form))))
                                   (explicit-pops ()
                                     `(+ (optimizer-explicit-pops optimizer)
                                         (optimizer-pushed-after-explicit-pops optimizer)))
                                   (explicit-pops? ()
                                     `(plusp (optimizer-explicit-pops optimizer))))
                          ,@body))))))
      (if (atom name)
          (define name)
          `(progn
             ,@(loop for name in name
                     collect (define name)))))))


;;; Various CL forms optimizers

#+TODO ;;---*** TODO: Doesn't work as the unoptimized SETF won't have STATE-SLOT-ADDRESS
(define-optimizer setf (optimizer form bindings)
  (destructuring-bind (word &rest places&values) form
    (declare (ignore word))
    (let ((slots (loop for place in places&values by #'cddr
                       when (and (listp place)
                                 (eq (first place) 'memory-cell)
                                 ;; (eq (second place) 'memory)
                                 (listp (third place))
                                 (eq (first (third place)) 'state-slot-address))
                         collect (third place))))
      (if slots
          (append (empty-optimizer-data-stack optimizer :contains slots) (list form))
          `((setf ,@(loop for (place value) on places&values by #'cddr
                          collect (optimize-expr optimizer place bindings)
                          collect (optimize-expr optimizer value bindings))))))))

(defmacro invisible-binding (form)
  form)

(define-optimizer (let let*) (optimizer form bindings)
  (destructuring-bind (word (&rest newbindings) &body body) form
    (multiple-value-bind (newvars newexprs invisibles?)
        (loop for (var expr) in newbindings
              collect var into vars
              if (and (listp expr) (eq (first expr) 'invisible-binding))
                collect (optimize-expr optimizer (second expr) bindings) into exprs
                and collect t into invisibles?
              else
                collect (optimize-expr optimizer expr bindings) into exprs
                and collect nil into invisibles?
              finally (return (values vars exprs invisibles?)))
      `((,word (,@(loop for var in newvars
                        for expr in newexprs
                        collect `(,var ,expr)))
               (declare (ignorable ,@newvars))
               ,@(loop with bindings = (append (mapcan #'(lambda (var expr invisible?)
                                                           (if invisible?
                                                               ;; If a variable is marked as an invisible-binding, don't
                                                               ;; record its value to avoid optimizing references to that
                                                               ;; variable as the variable is going to be modified in
                                                               ;; the body. E.g., optimizing (from ACCEPT's definition)
                                                               ;;   (setf nread (the fixnum (1+ nread)))
                                                               ;; to
                                                               ;;   (setf 0 (the fixnum (1+ 0)))
                                                               ;; NOTE: All Forth LOCALs are marked as invisible
                                                               `((,var ,+unknown-value+))
                                                               `((,var ,expr))))
                                                       newvars newexprs invisibles?)
                                               bindings)
                       for form in body
                       append (optimize-form optimizer form bindings))
               ,@(empty-optimizer-data-stack optimizer :contains newvars))))))

(define-optimizer multiple-value-bind (optimizer form bindings)
    (destructuring-bind (word (&rest newvars) expr &body body) form
      `((,word (,@newvars) ,(optimize-expr optimizer expr bindings)
               ,@(loop with bindings = (append (mapcar #'(lambda (v) (list v +unknown-value+)) newvars) bindings)
                       for form in body
                       append (optimize-form optimizer form bindings))
               ,@(empty-optimizer-data-stack optimizer :contains newvars)))))

(define-optimizer (when unless) (optimizer form bindings)
  (destructuring-bind (word test &body body) form
    (let ((invert? (eq word 'unless))
          (test (optimize-expr optimizer test bindings)))
      (flet ((check? (value)
               (if invert? (not value) value))
             (doit ()
               (loop for form in body
                     append (optimize-form optimizer form bindings t))))
        (cond ((atom test)
               (when (check? test)
                 (doit)))
              ((and (member (first test) '(plusp minusp zerop))
                    (numberp (second test)))
               (let ((predicate (first test))
                     (value (second test)))
                 (when (check? (funcall predicate value))
                   (doit))))
              ((and (equal test '(pictured-buffer-active? (memory-pictured-buffer memory)))
                    (not (eq (optimizer-pictured-buffer-active? optimizer) :unknown)))
               (when (check? (optimizer-pictured-buffer-active? optimizer))
                 (doit)))
              (t
               `((,word ,test
                        ,@(loop for form in body
                                append (optimize-form optimizer form bindings t))))))))))

(define-optimizer declare (optimizer form bindings)
  (declare (ignore optimizer bindings))
  (list form))

(define-optimizer tagbody (optimizer form bindings)
  `((tagbody
       ,@(loop for form in (rest form)
               append (optimize-form optimizer form bindings))
       ,@(empty-optimizer-data-stack optimizer))))

(define-optimizer if (optimizer form bindings)
  (destructuring-bind (word test then &optional else) form
    (if (or (and (listp then) (eq (first then) 'stack-push) (eq (second then) 'data-stack))
            (and (listp else) (eq (first else) 'stack-push) (eq (second else) 'data-stack)))
        ;; If either clause of the IF pushes something onto the data stack, we have to
        ;; flush the optimizer stack as we don't know which clause will be executed.
        ;; Consequently, we can't know the state of the data stack after the IF executes.
        (punt)
        `((,word ,(optimize-expr optimizer test bindings)
                 ,@(optimize-form optimizer then bindings)
                 ,@(when else
                     (optimize-form optimizer else bindings)))))))


;;; Forth non-stack optimizers

(define-optimizer start-pictured-buffer (optimizer form bindings)
  (declare (ignore bindings))
  (setf (optimizer-pictured-buffer-active? optimizer) t)
  (list form))

(define-optimizer finish-pictured-buffer (optimizer form bindings)
  (declare (ignore bindings))
  (setf (optimizer-pictured-buffer-active? optimizer) nil)
  (list form))


;;; Forth stack operations optimizers

(defun stack-pop? (form)
  (labels ((pop? (form)
             (cond ((atom form)
                    nil)
                   ((equal (car form) 'stack-pop))
                   (t
                    (or (pop? (car form)) (pop? (cdr form)))))))
    (pop? form)))

(define-optimizer stack-underflow-check (optimizer form bindings)
  (declare (ignore bindings))
  (destructuring-bind (word stack &optional (n 1)) form
    (declare (ignore word))
    (cond ((and (eq stack 'data-stack) (constantp n))
           (if (>= (+ (stack-depth (optimizer-data-stack optimizer)) (explicit-pops)) n)
               nil
               (list form)))
          ((and (eq stack 'return-stack) (constantp n))
           (if (>= (optimizer-return-stack-depth optimizer) n)
               nil
               (list form)))
          (t
           (list form)))))

(define-optimizer flush-optimizer-stack (optimizer form bindings)
  (declare (ignore bindings))
  (destructuring-bind (word &key contains depth) form
    (declare (ignore word))
    (empty-optimizer-data-stack optimizer :contains (and contains (list contains)) :depth depth)))

(define-optimizer stack-push (optimizer form bindings)
  (destructuring-bind (word stack value) form
    (declare (ignore word))
    (cond ((eq stack 'data-stack)
           (multiple-value-bind (expr empty-stack?)
               (optimize-expr optimizer value bindings)
             (cond (empty-stack?
                    (punt `(stack-push data-stack ,expr)))
                   ((and (equal expr '(stack-pop return-stack))
                         (labels ((return-stack-ref? (expr)
                                    (cond ((atom expr) nil)
                                          ((and (listp expr)
                                                (eq (first expr) 'stack-cell)
                                                (eq (second expr) 'return-stack)))
                                          (t
                                           (or (return-stack-ref? (car expr)) (return-stack-ref? (cdr expr)))))))
                           (loop for cell across (stack-contents (optimizer-data-stack optimizer))
                                   thereis (return-stack-ref? cell))))
                    (punt `(stack-push data-stack ,expr)))
                   (t
                    (stack-push (optimizer-data-stack optimizer) expr)
                    (when (plusp (optimizer-explicit-pops optimizer))
                      (incf (optimizer-pushed-after-explicit-pops optimizer)))
                    nil))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list `(stack-push return-stack ,(optimize-expr optimizer value bindings))))
          (t
           (list `(stack-push ,stack ,(optimize-expr optimizer value bindings)))))))

(define-optimizer stack-push-double (optimizer form bindings)
  (destructuring-bind (word stack value) form
    (cond ((eq stack 'data-stack)
           (multiple-value-bind (expr empty-stack?)
               (optimize-expr optimizer value bindings)
             (cond (empty-stack?
                    (punt `(,word data-stack ,expr)))
                   ((numberp expr)
                    (prog1
                        nil
                      (stack-push-double (optimizer-data-stack optimizer) expr)
                      (when (plusp (optimizer-explicit-pops optimizer))
                        (incf (optimizer-pushed-after-explicit-pops optimizer) 2))))
                   (t
                    ;; We don't have a mechanism to note that an entry on the optimizer stack
                    ;; is actually a double and should count as 2 entries. So, just flush the stack.
                    (punt `(,word data-stack ,expr))))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer) 2)
           (list `(stack-push-double return-stack ,(optimize-expr optimizer value bindings))))
          (t
           (list `(stack-push-double ,stack ,(optimize-expr optimizer value bindings)))))))

(define-optimizer stack-pop (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((plusp (optimizer-pushed-after-explicit-pops optimizer))
                  (prog1
                      nil
                    (stack-pop (optimizer-data-stack optimizer))
                    (decf (optimizer-pushed-after-explicit-pops optimizer))))
                 ((plusp (optimizer-explicit-pops optimizer))
                  (prog1
                      (list form)
                    (decf (optimizer-explicit-pops optimizer))))
                 ((plusp (stack-depth (optimizer-data-stack optimizer)))
                  (prog1
                      nil
                    (stack-pop (optimizer-data-stack optimizer))))
                 (t
                  (list form))))
          ((eq (second form) 'return-stack)
           (when (plusp (optimizer-return-stack-depth optimizer))
             (decf (optimizer-return-stack-depth optimizer)))
           (list form))
          (t
           (list form)))))

(define-optimizer (stack-pop-double stack-pop-double-unsigned) (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (let ((count 2)
                 (forms nil))
             (when (plusp (optimizer-pushed-after-explicit-pops optimizer))
               (cond ((>= (optimizer-pushed-after-explicit-pops optimizer) 2)
                      (stack-pop-double (optimizer-data-stack optimizer))
                      (decf (optimizer-pushed-after-explicit-pops optimizer) 2)
                      (decf count 2))
                     (t
                      ;; Only one push after explicit pops
                      (stack-pop (optimizer-data-stack optimizer))
                      (decf (optimizer-pushed-after-explicit-pops optimizer))
                      (decf count))))
             (when (and (plusp count) (plusp (optimizer-explicit-pops optimizer)))
               (let ((n (min count (optimizer-explicit-pops optimizer))))
                 (if (= n 2)
                     (push form forms)
                     (push '(stack-pop data-stack) forms))
                 (decf (optimizer-explicit-pops optimizer) n)
                 (decf count n)))
             (when (plusp count)
               (cond ((>= (stack-depth (optimizer-data-stack optimizer)) count)
                      (if (= count 2)
                          (stack-pop-double (optimizer-data-stack optimizer))
                          (stack-pop (optimizer-data-stack optimizer))))
                     ((zerop (stack-depth (optimizer-data-stack optimizer)))
                      (push form forms))
                     (t
                      ;; count was 2 and optimizer stack depth was 1
                      (stack-pop (optimizer-data-stack optimizer))
                      (push '(stack-pop data-stack) forms))))
             (reverse forms)))
          ((eq (second form) 'return-stack)
           (when (plusp (optimizer-return-stack-depth optimizer))
             (decf (optimizer-return-stack-depth optimizer) 2)
             (when (minusp (optimizer-return-stack-depth optimizer))
               (setf (optimizer-return-stack-depth optimizer) 0)))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-drop (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (if (plusp (optimizer-pushed-after-explicit-pops optimizer))
                      (if (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                          (punt)
                          (prog1
                              nil
                            (stack-drop (optimizer-data-stack optimizer))
                            (decf (optimizer-pushed-after-explicit-pops optimizer))))
                      (prog1
                          (list form)
                        (decf (optimizer-explicit-pops optimizer)))))
                 ((zerop (stack-depth (optimizer-data-stack optimizer)))
                  (list form))
                 ((stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-drop (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (when (plusp (optimizer-return-stack-depth optimizer))
             (decf (optimizer-return-stack-depth optimizer)))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-dup (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((zerop (stack-depth (optimizer-data-stack optimizer)))
                  (list form))
                 ((stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-dup (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list form))
          (t
           (list form)))))

;;;---*** TODO: Can we do better?
(define-optimizer stack-?dup (optimizer form bindings)
  (declare (ignore bindings))
  (if (eq (second form) 'data-stack)
      (punt)
      (list form)))
  
(define-optimizer stack-nip (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 2)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-nip (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (when (plusp (optimizer-return-stack-depth optimizer))
             (decf (optimizer-return-stack-depth optimizer)))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-over (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 2)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-over (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-pick (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (let ((n (third form)))
             (cond ((not (constantp n))
                    (punt))
                   ((explicit-pops?)
                    (punt))
                   ((< (stack-depth (optimizer-data-stack optimizer)) n)
                    (punt))
                   ((loop for i below n
                            thereis (stack-pop? (stack-cell (optimizer-data-stack optimizer) i)))
                    (punt))
                   (t
                    (prog1
                        nil
                      (stack-pick (optimizer-data-stack optimizer) n))))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (punt-if-data-stack-pop (third form)))
          (t
           (punt-if-data-stack-pop (third form))))))

(define-optimizer stack-roll (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (let ((n (third form)))
             (cond ((not (constantp n))
                    (punt))
                   ((explicit-pops?)
                    (punt))
                   ((< (stack-depth (optimizer-data-stack optimizer)) n)
                    (punt))
                   ((loop for i below n
                            thereis (stack-pop? (stack-cell (optimizer-data-stack optimizer) i)))
                    (punt))
                   (t
                    (prog1
                        nil
                      (stack-roll (optimizer-data-stack optimizer) n))))))
          ;; Don't have to special case the RETURN-STACK as this operation doesn't change stack depth
          (t
           (punt-if-data-stack-pop (third form))))))

(define-optimizer stack-rot (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 3)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 2)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-rot (optimizer-data-stack optimizer))))))
          (t
           (list form)))))

(define-optimizer stack-rot-down (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 3)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 2)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-rot-down (optimizer-data-stack optimizer))))))
          (t
           (list form)))))

(define-optimizer stack-swap (optimizer form bindings)
  (declare (ignore bindings))
  (if (eq (second form) 'data-stack)
      (cond ((explicit-pops?)
             (punt))
            ((< (stack-depth (optimizer-data-stack optimizer)) 2)
             (punt))
            ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                 (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1)))
             (punt))
            (t
             (prog1
                 nil
               (stack-swap (optimizer-data-stack optimizer)))))
      (list form)))

(define-optimizer stack-tuck (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 2)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-tuck (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-2drop (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  ;;---*** TODO: Can do a bit better here (see stack-drop optimizer)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 2)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-2drop (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (when (plusp (optimizer-return-stack-depth optimizer))
             (decf (optimizer-return-stack-depth optimizer) 2))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-2dup (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((explicit-pops?)
                  (punt))
                 ((< (stack-depth (optimizer-data-stack optimizer)) 2)
                  (punt))
                 ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                      (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1)))
                  (punt))
                 (t
                  (prog1
                      nil
                    (stack-2dup (optimizer-data-stack optimizer))))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer) 2)
           (list form))
          (t
           (list form)))))

;;;---*** TODO: stack-2over
;;;---*** TODO: stack-2rot

(define-optimizer stack-2swap (optimizer form bindings)
  (declare (ignore bindings))
  (if (eq (second form) 'data-stack)
      (cond ((explicit-pops?)
             (punt))
            ((< (stack-depth (optimizer-data-stack optimizer)) 4)
             (punt))
            ((or (stack-pop? (stack-cell (optimizer-data-stack optimizer) 0))
                 (stack-pop? (stack-cell (optimizer-data-stack optimizer) 1))
                 (stack-pop? (stack-cell (optimizer-data-stack optimizer) 2))
                 (stack-pop? (stack-cell (optimizer-data-stack optimizer) 3)))
             (punt))
            (t
             (prog1
                 nil
               (stack-2swap (optimizer-data-stack optimizer)))))
      (list form)))

(define-optimizer stack-snip (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (let ((n (third form)))
             (cond ((not (constantp n))
                    (punt))
                   ((explicit-pops?)
                    (punt))
                   ((< (stack-depth (optimizer-data-stack optimizer)) n)
                    (punt))
                   ((loop for i below n
                            thereis (stack-pop? (stack-cell (optimizer-data-stack optimizer) i)))
                    (punt))
                   (t
                    (prog1
                        nil
                      (stack-snip (optimizer-data-stack optimizer) n))))))
          ;; Don't have to special case the RETURN-STACK as this operation doesn't change stack depth
          (t
           (punt-if-data-stack-pop (third form))))))

(define-optimizer stack-ndrop (optimizer form bindings)
  (declare (ignore bindings))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (let ((n (third form)))
             (cond ((not (constantp n))
                    (punt))
                   ((explicit-pops?)
                    (punt))
                   ((< (stack-depth (optimizer-data-stack optimizer)) n)
                    (punt))
                   ((loop for i below n
                            thereis (stack-pop? (stack-cell (optimizer-data-stack optimizer) i)))
                    (punt))
                   (t
                    (prog1
                        nil
                      (stack-ndrop (optimizer-data-stack optimizer) n))))))
          ((eq stack 'return-stack)
           (let ((n (third form)))
             (when (constantp n)
               (if (> n (optimizer-return-stack-depth optimizer))
                   (decf (optimizer-return-stack-depth optimizer) n)
                   (setf (optimizer-return-stack-depth optimizer) 0))))
           (punt-if-data-stack-pop (third form)))
          (t
           (punt-if-data-stack-pop (third form))))))


;;; Optimizer top-level

(defun optimize-form (optimizer form bindings &optional conditional?)
  (handler-bind ((forth-exception
                   #'(lambda (exception)
                       (when (member (forth-exception-key exception) '(:optimizer-stack-overflow :optimizer-stack-underflow))
                         (format t "~A while optimizing ~S~%" (forth-exception-phrase exception) form)
                         (return-from optimize-form (list form))))))
    (flet ((optimize ()
             (flet ((punt ()
                      (append (empty-optimizer-data-stack optimizer) (list form)))
                    (default-optimizer (optimizer form bindings)
                      (list (optimize-expr optimizer form bindings))))
               (cond ((atom form)
                      ;; Flush the optimizer stack before the target of a transfer of control to ensure
                      ;; the data stack has the expected contents.
                      (punt))
                     ((eq (first form) 'forth-call)
                      ;; Flush the optimizer stack before calling another definition to ensure the
                      ;; data stack contents are as that definition would expect.
                      (punt))
                     ((eq (first form) 'go)
                      ;; Flush the optimizer stack before transferring control to another point in the
                      ;; definition. But, if the GO is inside a conditional, preserve the optimizer
                      ;; stack contents for the forms after the conditional expression.
                      (append (empty-optimizer-data-stack optimizer :conditional? conditional?) (list form)))
                     (t
                      (funcall (gethash (first form) *optimizers* #'default-optimizer) optimizer form bindings))))))
      (catch 'empty-and-retry
        (return-from optimize-form (optimize)))
      ;; If we get here, OPTIMIZE-EXPR detected a situation that requires
      ;; we empty the optimizer stack before optimizing this form.
      (append (empty-optimizer-data-stack optimizer) (optimize)))))

(defun optimize-definition (forms)
  (let* ((optimizer (make-optimizer))
         (optimized (loop for form in forms
                          append (optimize-form optimizer form nil))))
    (append optimized (empty-optimizer-data-stack optimizer))))
