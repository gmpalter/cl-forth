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

#||
(defparameter +not-optimized-marker+ '#:not-optimized)

(defun pop-optimizer-stack (optimizer-stack)
  (let ((expr (stack-pop optimizer-stack)))
    (if (eq expr +not-optimized-marker+)
        `(stack-pop data-stack)
        expr)))
  
(defun optimize-expr (optimizer-stack topexpr vars)
  (let ((substituted? nil))
  (labels ((optimize (expr)
             (cond ((atom expr)
                    (if (member expr vars)
                        (return-from optimize-expr (values topexpr t))
                        expr))
                   ((and (eq (first expr) 'stack-pop) (eq (second expr) 'data-stack))
                    (if (zerop (stack-depth optimizer-stack))
                        (if substituted?
                            expr
                            (return-from optimize-expr (values topexpr t)))
                        (prog1
                            (pop-optimizer-stack optimizer-stack)
                          (setf substituted? t))))
                   (t
                    (loop for subexpr in expr
                          collect (optimize subexpr))))))
    (optimize topexpr))))

(defun optimize-stack-push (optimizer-stack form vars)
  (multiple-value-bind (expr push-marker?)
      (optimize-expr optimizer-stack (third form) vars)
    (cond (push-marker?
           (stack-push optimizer-stack +not-optimized-marker+)
           (list form))
          (t
           (stack-push optimizer-stack expr)
           nil))))

(defun optimize-let (optimizer-stack form vars)
  (destructuring-bind (word (&rest bindings) &body body) form
    (multiple-value-bind (newvars exprs)
        (loop for (var expr) in bindings
              collect var into vars
              collect expr into exprs
              finally (return (values vars exprs)))
      `((,word (,@(loop for var in newvars
                        for expr in exprs
                        collect `(,var ,(optimize-expr optimizer-stack expr vars))))
               ,@(loop with vars = (append vars newvars)
                       for form in body
                       append (optimize-form optimizer-stack form vars)))))))

(defun optimize-when/unless (optimizer-stack form vars)
  (destructuring-bind (word test &body body) form
    `((,word ,(optimize-expr optimizer-stack test vars)
             ,@(loop for form in body
                     append (optimize-form optimizer-stack form vars))))))

(defun optimize-form (optimizer-stack form vars)
  (handler-bind ((forth-exception
                   #'(lambda (exception)
                       (when (eq (forth-exception-key exception) :optimizer-stack-overflow)
                         (return-from optimize-form form)))))
    (flet ((empty-optimizer-stack (form)
             (nconc (reverse (loop while (plusp (stack-depth optimizer-stack))
                                   append `((stack-push data-stack ,(pop-optimizer-stack optimizer-stack)))))
                    (list form))))
      (cond ((atom form)
             ;;---*** TODO: EXPLAIN
             (empty-optimizer-stack form))
            ((eq (first form) 'forth-call)
             ;;---*** TODO: EXPLAIN
             (empty-optimizer-stack form))
            ((eq (first form) 'go)
             ;;---*** TODO: EXPLAIN
             (empty-optimizer-stack form))
            ((and (eq (first form) 'stack-push) (eq (second form) 'data-stack))
             (optimize-stack-push optimizer-stack form vars))
            ((and (eq (first form) 'stack-pop) (eq (second form) 'data-stack))
             ;; STACK-POP as a top-level form is highly unlikely but ...
             (if (zerop (stack-depth optimizer-stack))
                 (list form)
                 (prog1
                     nil
                   (pop-optimizer-stack optimizer-stack))))
            ((and (eq (first form) 'stack-dup) (eq (second form) 'data-stack))
             (if (zerop (stack-depth optimizer-stack))
                 (list form)
                 (prog1
                     nil
                   (stack-push optimizer-stack (stack-cell optimizer-stack 0)))))
            ((or (eq (first form) 'let) (eq (first form) 'let*))
             (optimize-let optimizer-stack form vars))
            ((or (eq (first form) 'when) (eq (first form) 'unless))
             (optimize-when/unless optimizer-stack form vars))
            (t
             (list (optimize-expr optimizer-stack form vars)))))))

(defun optimize-definition (forms)
  (let* ((optimizer-stack (make-stack "Optimizer" 1024 :optimizer-stack-overflow :optimizer-stack-underflow))
         (optimized (loop for form in forms
                          append (optimize-form optimizer-stack form nil))))
    (append optimized (reverse (loop while (plusp (stack-depth optimizer-stack))
                                     append `((stack-push data-stack ,(pop-optimizer-stack optimizer-stack))))))))
||#

(defun optimize-definition (forms)
  forms)
