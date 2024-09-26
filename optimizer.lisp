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

(defparameter +not-optimized-marker+ '#:not-optimized)

(defstruct optimizer
  (data-stack (make-stack "Optimizer" 1024 :optimizer-stack-overflow :optimizer-stack-underflow)    )
  (return-stack-depth 0))

(defvar *optimizers* (make-hash-table))

(defmacro define-optimizer (name (optimizer form vars) &body body)
  (flet ((define (name)
           (let ((optimizer-fun (intern (format nil "OPTIMIZE-~A" name))))
             `(setf (gethash ',name *optimizers*)
                    (named-lambda ,optimizer-fun (,optimizer ,form ,vars)
                      ,@body)))))
    (if (atom name)
        (define name)
        `(progn
           ,@(loop for name in name
                   collect (define name))))))

(defun pop-optimizer-data-stack (optimizer)
  (let ((expr (stack-pop (optimizer-data-stack optimizer))))
    (if (eq expr +not-optimized-marker+)
        `(stack-pop data-stack)
        expr)))
  
(defun empty-optimizer-data-stack (optimizer)
  (when (plusp (stack-depth (optimizer-data-stack optimizer)))
    (let ((not-optimized-count (reduce #'+ (stack-contents (optimizer-data-stack optimizer))
                                       :initial-value 0
                                       :key #'(lambda (cell)
                                                (if (eq cell +not-optimized-marker+) 1 0)))))
      ;;---*** TODO
      (declare (ignore not-optimized-count))
      (reverse (loop while (plusp (stack-depth (optimizer-data-stack optimizer)))
                     for expr = (pop-optimizer-data-stack optimizer)
                     unless (equal expr '(stack-pop data-stack))
                       collect`(stack-push data-stack ,expr))))))

(defun optimize-expr (optimizer topexpr vars)
  (let ((substituted? nil))
    (labels ((optimize (expr)
               (cond ((atom expr)
                      (if (member expr vars)
                          (return-from optimize-expr (values topexpr t))
                          expr))
                     ((and (eq (first expr) 'stack-pop) (eq (second expr) 'data-stack))
                      (if (zerop (stack-depth (optimizer-data-stack optimizer)))
                          (if substituted?
                              expr
                              (return-from optimize-expr (values topexpr t)))
                          (prog1
                              (pop-optimizer-data-stack optimizer)
                            (setf substituted? t))))
                     (t
                      (loop for subexpr in expr
                            collect (optimize subexpr))))))
      (optimize topexpr))))

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
                       append (optimize-form optimizer form vars)))))))

(define-optimizer (when unless) (optimizer form vars)
  (destructuring-bind (word test &body body) form
    `((,word ,(optimize-expr optimizer test vars)
             ,@(loop for form in body
                     append (optimize-form optimizer form vars))))))

(define-optimizer multiple-value-bind (optimizer form vars)
  (destructuring-bind (word (&rest newvars) expr &body body) form
    `((,word (,@newvars) ,(optimize-expr optimizer expr vars)
             ,@(loop with vars = (append vars newvars)
                     for form in body
                     append (optimize-form optimizer form vars))))))

(define-optimizer stack-underflow-check (optimizer form vars)
  (declare (ignore vars))
  (destructuring-bind (word stack &optional (n 1)) form
    (declare (ignore word))
    (cond ((eq stack 'data-stack)
           (if (>= (stack-depth (optimizer-data-stack optimizer)) n)
               nil
               (list form)))
          ((eq stack 'return-stack)
           (if (>= (optimizer-return-stack-depth optimizer) n)
               nil
               (list form)))
          (t
           (list form)))))

(define-optimizer stack-push (optimizer form vars)
  (destructuring-bind (word stack value) form
    (declare (ignore word))
    (cond ((eq stack 'data-stack)
           (multiple-value-bind (expr push-marker?)
               (optimize-expr optimizer value vars)
             (cond (push-marker?
                    (stack-push (optimizer-data-stack optimizer) +not-optimized-marker+)
                    (list form))
                   (t
                    (stack-push (optimizer-data-stack optimizer) expr)
                    nil))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list `(stack-push return-stack ,(optimize-expr optimizer value vars))))
          (t
           (list `(stack-push ,stack ,(optimize-expr optimizer value vars)))))))

(define-optimizer stack-pop (optimizer form vars)
  (declare (ignore vars))
  (when (and (eq (second form) 'return-stack)
             (plusp (optimizer-return-stack-depth optimizer)))
    (decf (optimizer-return-stack-depth optimizer)))
  (list form))

(define-optimizer stack-dup (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (if (zerop (stack-depth (optimizer-data-stack optimizer)))
               (list form)
               (prog1
                   (and (eq (stack-cell (optimizer-data-stack optimizer) 0) +not-optimized-marker+) (list form))
                 (stack-dup (optimizer-data-stack optimizer)))))
          ((eq stack 'return-stack)
           (incf (optimizer-return-stack-depth optimizer))
           (list form))
          (t
           (list form)))))

(define-optimizer stack-?dup (optimizer form vars)
  (declare (ignore vars))
  (if (eq (second form) 'data-stack)
      (append (empty-optimizer-data-stack optimizer) (list form))
      (list form)))

(define-optimizer stack-drop (optimizer form vars)
  (declare (ignore vars))
  (let ((stack (second form)))
    (cond ((eq stack 'data-stack)
           (if (zerop (stack-depth (optimizer-data-stack optimizer)))
               (list form)
               (let ((expr (pop-optimizer-data-stack optimizer)))
                 (if (constantp expr)
                     nil
                     (append (empty-optimizer-data-stack optimizer) (list expr))))))
          ((eq stack 'return-stack)
           (decf (optimizer-return-stack-depth optimizer))
           (list form))
          (t
           (list form)))))
  
(define-optimizer stack-swap (optimizer form vars)
  (declare (ignore vars))
  (if (eq (second form) 'data-stack)
      (if (> (stack-depth (optimizer-data-stack optimizer)) 1)
          (prog1
              nil
            (stack-swap (optimizer-data-stack optimizer)))
          (append (empty-optimizer-data-stack optimizer) (list form)))
      (list form)))

;;;

(defun optimize-form (optimizer form vars)
  (handler-bind ((forth-exception
                   #'(lambda (exception)
                       (when (member (forth-exception-key exception) '(:optimizer-stack-overflow :optimizer-stack-underflow))
                         (format t "~A while optimizing ~S~%" (forth-exception-phrase exception) form)
                         (return-from optimize-form (list form))))))
    (flet ((punt ()
             (nconc (empty-optimizer-data-stack optimizer) (list form)))
           (default-optimizer (optimizer form vars)
             (list (optimize-expr optimizer form vars))))
      (cond ((atom form)
             ;;---*** TODO: EXPLAIN
             (punt))
            ((eq (first form) 'forth-call)
             ;;---*** TODO: EXPLAIN
             (punt))
            ((eq (first form) 'go)
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
