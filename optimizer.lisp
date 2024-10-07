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
  (return-stack-depth 0))

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
  
(defun empty-optimizer-data-stack (optimizer &key vars count)
  (declare (ignore count))
  (let* ((optimizer-stack (optimizer-data-stack optimizer))
         (stack-depth (stack-depth optimizer-stack)))
    (labels ((references? (expr)
               (if (atom expr)
                   (member expr vars)
                   (or (references? (car expr)) (references? (cdr expr)))))
             (reset ()
               (setf (optimizer-explicit-pops optimizer) 0
                     (optimizer-pushed-after-explicit-pops optimizer) 0))
             (empty-stack (count)
               (reverse (loop for i below count
                              collect `(stack-push data-stack ,(stack-pop optimizer-stack))))))
      (cond ((zerop stack-depth)
             (reset)
             nil)
            ((plusp (optimizer-explicit-pops optimizer))
             (let* ((pops (optimizer-explicit-pops optimizer))
                    (extras (optimizer-pushed-after-explicit-pops optimizer))
                    (extras-forms (when (plusp extras)
                                    (prog1
                                        (empty-stack extras)
                                      (decf stack-depth extras)))))
               (reset)
               (prog1
                   (append (loop for i below stack-depth
                                 for cell = (stack-cell optimizer-stack (- stack-depth i 1))
                                 collect `(stack-push data-stack ,cell)
                                 collect `(stack-roll-down data-stack ,(+ pops i)))
                           extras-forms)
                 (setf (stack-depth optimizer-stack) 0))))
            (vars
             (reset)
             ;; Find the deepest entry that references one of the variables and
             ;; pop it and everything after it from the stack.
             (let ((count (loop for i below stack-depth
                                for cell = (stack-cell optimizer-stack (- stack-depth i 1))
                                when (references? cell)
                                  return (- stack-depth i)
                                finally (return 0))))
               (when (plusp count)
                 (prog1
                     (empty-stack count)
                   (setf (optimizer-explicit-pops optimizer) count)))))
            ;;---*** TODO: This needs more thought
            #+TODO
            ((and count (< count stack-depth))
             (reset)
             ;; Flush a specific number of forms off the stack.
             (prog1
                 (empty-stack count)
               (setf (optimizer-explicit-pops optimizer) count)))
            (t
             (reset)
             ;; If there are no vars or explicit count, pop the entire stack.
             (empty-stack stack-depth))))))

(defun empty-optimizer-data-stack-containing (optimizer contents)
  (labels ((references? (expr)
             (cond ((member expr contents :test #'equal))
                   ((atom expr) nil)
                   (t (or (references? (car expr)) (references? (cdr expr)))))))
    (when (loop for cell across (stack-contents (optimizer-data-stack optimizer))
                  thereis (references? cell))
      (empty-optimizer-data-stack optimizer))))

;;;

(defun optimize-expr (optimizer topexpr vars)
  (let ((substituted? nil))
    (labels ((optimize (expr toplevel?)
               (cond ((atom expr)
                      (if (and (member expr vars) (not toplevel?))
                          (return-from optimize-expr topexpr)
                          expr))
                     ((and (eq (first expr) 'stack-pop) (eq (second expr) 'data-stack))
                      (if (zerop (stack-depth (optimizer-data-stack optimizer)))
                          (if substituted?
                              expr
                              (return-from optimize-expr topexpr))
                          (prog1
                              (pop-optimizer-data-stack optimizer)
                            (setf substituted? t))))
                     ((and (eq (first expr) 'stack-pop) (eq (second expr) 'return-stack))
                      (prog1
                          expr
                        (when (plusp (optimizer-return-stack-depth optimizer))
                          (decf (optimizer-return-stack-depth optimizer)))))
                     #+TODO
                     ((and (eq (first expr) 'stack-cell) (eq (second expr) 'data-stack) (constantp (third expr))
                           (< (third expr) (stack-depth (optimizer-data-stack optimizer)))
                           ;;---*** TODO: Can we do better than this?
                           (zerop (optimizer-explicit-pops optimizer))
                           (zerop (optimizer-pushed-after-explicit-pops optimizer)))
                      (stack-cell (optimizer-data-stack optimizer) (third expr)))
                     (t
                      (loop for subexpr in expr
                            collect (optimize subexpr nil))))))
      (optimize topexpr t))))

;;;

(defvar *optimizers* (make-hash-table))

(defmacro define-optimizer (name (optimizer form vars) &body body)
  (multiple-value-bind (body declarations doc)
      (uiop:parse-body body)
    (declare (ignore doc))
    (flet ((define (name)
             (let ((optimizer-fun (intern (format nil "OPTIMIZE-~A" name))))
               `(setf (gethash ',name *optimizers*)
                      (named-lambda ,optimizer-fun (,optimizer ,form ,vars)
                        ,@declarations
                        (macrolet ((punt ()
                                     `(append (empty-optimizer-data-stack optimizer) (list form))))
                          ,@body))))))
      (if (atom name)
          (define name)
          `(progn
             ,@(loop for name in name
                     collect (define name)))))))

#+TODO ;;---*** TODO: Doesn't work as the unoptimized SETF won't have STATE-SLOT-ADDRESS
(define-optimizer setf (optimizer form vars)
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
          (append (empty-optimizer-data-stack-containing optimizer slots) (list form))
          `((setf ,@(loop for (place value) on places&values by #'cddr
                          collect (optimize-expr optimizer place vars)
                          collect (optimize-expr optimizer value vars))))))))

(define-optimizer (let let*) (optimizer form vars)
  (destructuring-bind (word (&rest bindings) &body body) form
    (multiple-value-bind (newvars exprs)
        (loop for (var expr) in bindings
              collect var into vars
              collect expr into exprs
              finally (return (values vars exprs)))
      `((,word (,@(loop for var in newvars
                        for expr in exprs
                        collect `(,var ,(optimize-expr optimizer expr vars))))
               ,@(loop with vars = (append vars newvars)
                       for form in body
                       append (optimize-form optimizer form vars))
               ,@(empty-optimizer-data-stack optimizer :vars newvars))))))

(define-optimizer multiple-value-bind (optimizer form vars)
    (destructuring-bind (word (&rest newvars) expr &body body) form
      `((,word (,@newvars) ,(optimize-expr optimizer expr vars)
               ,@(loop with vars = (append vars newvars)
                       for form in body
                       append (optimize-form optimizer form vars))
               ,@(empty-optimizer-data-stack optimizer :vars newvars)))))

(define-optimizer (when unless) (optimizer form vars)
  (destructuring-bind (word test &body body) form
    `((,word ,(optimize-expr optimizer test vars)
             ,@(loop for form in body
                     append (optimize-form optimizer form vars t))))))

(define-optimizer declare (optimizer form vars)
  (declare (ignore optimizer vars))
  (list form))

(define-optimizer tagbody (optimizer form vars)
  `((tagbody
       ,@(loop for form in (rest form)
               append (optimize-form optimizer form vars))
       ,@(empty-optimizer-data-stack optimizer))))

;;;

(defun stack-pop? (form)
  (labels ((pop? (form)
             (cond ((atom form)
                    nil)
                   ((equal (car form) 'stack-pop))
                   (t
                    (or (pop? (car form)) (pop? (cdr form)))))))
    (pop? form)))

(define-optimizer stack-underflow-check (optimizer form vars)
  (declare (ignore vars))
  (destructuring-bind (word stack &optional (n 1)) form
    (declare (ignore word))
    (cond ((and (eq stack 'data-stack) (constantp n))
           (if (>= (stack-depth (optimizer-data-stack optimizer)) n)
               nil
               (list form)))
          ((and (eq stack 'return-stack) (constantp n))
           (if (>= (optimizer-return-stack-depth optimizer) n)
               nil
               (list form)))
          (t
           (list form)))))

(define-optimizer flush-optimizer-stack (optimizer form vars)
  (declare (ignore vars))
  (destructuring-bind (word &key count contains) form
    (declare (ignore word))
    (if contains
        (empty-optimizer-data-stack-containing optimizer (list contains))
        (empty-optimizer-data-stack optimizer :count count))))
  
(define-optimizer stack-push (optimizer form vars)
  (destructuring-bind (word stack value) form
    (declare (ignore word))
    (cond ((eq stack 'data-stack)
           (multiple-value-bind (expr empty-stack?)
               (optimize-expr optimizer value vars)
             (cond (empty-stack?
                    (punt))
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
                    (punt))
                   (t
                    (stack-push (optimizer-data-stack optimizer) expr)
                    (when (plusp (optimizer-explicit-pops optimizer))
                      (incf (optimizer-pushed-after-explicit-pops optimizer)))
                    nil))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list `(stack-push return-stack ,(optimize-expr optimizer value vars))))
          (t
           (list `(stack-push ,stack ,(optimize-expr optimizer value vars)))))))

;;; stack-push-double

(define-optimizer stack-pop (optimizer form vars)
  (declare (ignore vars))
  (if (and (eq (second form) 'return-stack)
           (plusp (optimizer-return-stack-depth optimizer)))
      (prog1
          (list form)
        (decf (optimizer-return-stack-depth optimizer)))
      (list form)))

;;; stack-pop-double
;;; stack-pop-double-unsigned

(define-optimizer stack-drop (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((zerop (stack-depth (optimizer-data-stack optimizer)))
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

(define-optimizer stack-dup (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((zerop (stack-depth (optimizer-data-stack optimizer)))
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
(define-optimizer stack-?dup (optimizer form vars)
  (declare (ignore vars))
  (if (eq (second form) 'data-stack)
      (punt)
      (list form)))
  
;;; stack-nip

(define-optimizer stack-over (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((< (stack-depth (optimizer-data-stack optimizer)) 2)
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

;;; stack-pick
;;; stack-roll

(define-optimizer stack-rot (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((< (stack-depth (optimizer-data-stack optimizer)) 3)
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

(define-optimizer stack-rot-down (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((< (stack-depth (optimizer-data-stack optimizer)) 3)
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

(define-optimizer stack-swap (optimizer form vars)
  (declare (ignore vars))
  (if (eq (second form) 'data-stack)
      (if (>= (stack-depth (optimizer-data-stack optimizer)) 2)
          (prog1
              nil
            (stack-swap (optimizer-data-stack optimizer)))
          (punt))
      (list form)))

;;; stack-tuck

(define-optimizer stack-2drop (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((< (stack-depth (optimizer-data-stack optimizer)) 2)
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

(define-optimizer stack-2dup (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (cond ((< (stack-depth (optimizer-data-stack optimizer)) 2)
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

;;; stack-2over
;;; stack-2rot

(define-optimizer stack-2swap (optimizer form vars)
  (declare (ignore vars))
  (if (eq (second form) 'data-stack)
      (if (>= (stack-depth (optimizer-data-stack optimizer)) 4)
          (prog1
              nil
            (stack-swap (optimizer-data-stack optimizer)))
          (punt))
      (list form)))

;;; stack-snip

;;;

(defun optimize-form (optimizer form vars &optional conditional?)
  (handler-bind ((forth-exception
                   #'(lambda (exception)
                       (when (member (forth-exception-key exception) '(:optimizer-stack-overflow :optimizer-stack-underflow))
                         (format t "~A while optimizing ~S~%" (forth-exception-phrase exception) form)
                         (return-from optimize-form (list form))))))
    (flet ((punt ()
             (append (empty-optimizer-data-stack optimizer) (list form)))
           (default-optimizer (optimizer form vars)
             (list (optimize-expr optimizer form vars))))
      (cond ((atom form)
             ;;---*** TODO: EXPLAIN
             (punt))
            ((eq (first form) 'forth-call)
             ;;---*** TODO: EXPLAIN
             (punt))
            ((and (eq (first form) 'go) (not conditional?))
             ;;---*** TODO: EXPLAIN
             (punt))
            (t
             (funcall (gethash (first form) *optimizers* #'default-optimizer) optimizer form vars))))))

(defun optimize-definition (forms)
  (let* ((optimizer (make-optimizer))
         (optimized (loop for form in forms
                          append (optimize-form optimizer form nil))))
    (append optimized (empty-optimizer-data-stack optimizer))))

;;(defun optimize-definition (forms)
;;  forms)
