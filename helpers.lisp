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

;;; Fast integer printing

(define-constant +digits+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defconstant +max-width+ 130)

(defun write-integer (value base)
  (declare (type integer value) (fixnum base)
           (optimize (speed 3) (safety 0)))
  (let ((text (make-string +max-width+ :element-type 'base-char))
        (start (1- +max-width+))
        (negative? (minusp value))
        (rem 0))
    (declare (fixnum start rem)
             (dynamic-extent text))
    (when negative?
      (setf value (- value)))
    (if (fixnump value)
        (locally (declare (fixnum value))
          (loop
            (multiple-value-setq (value rem) (truncate value base))
            (setf (schar text start) (schar +digits+ rem))
            (when (zerop value)
              (return))
            (setf start (the fixnum (1- start)))))
        (loop
          (multiple-value-setq (value rem) (truncate value base))
          (setf (schar text start) (schar +digits+ rem))
          (when (zerop value)
            (return))
          (setf start (the fixnum (1- start)))))
    (when negative?
      (setf start (the fixnum (1- start)))
      (setf (schar text start) #\-))
    (write-string text *standard-output* :start start :end +max-width+)
    (write-char #\Space)
    nil))


;;; Functions used as the code for some of the words defined by Forth 2012

(defun push-parameter-as-cell (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push data-stack (parameters-p1 parameters))))

(defun push-parameter-as-double-cell (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push-double data-stack (parameters-p1 parameters))))

(defun push-value (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (let ((address (parameters-p1 parameters))
          (type (parameters-p2 parameters)))
      (case type
        (:value
         (stack-push data-stack (memory-cell memory address)))
        (:2value
         (stack-push-double data-stack (memory-double-cell memory address)))
        (:fvalue
         (stack-push float-stack (memory-native-float memory address)))))))

(defun push-parameter-as-float (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push float-stack (parameters-p1 parameters))))

(defun execute-parameter (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (when (null (parameters-p1 parameters))
      (forth-exception :defer-not-set))
    (execute execution-tokens (parameters-p1 parameters) fs)))

(defun do-marker (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (execute-marker word-lists execution-tokens files (parameters-p1 parameters))))

(defun replace-top-of-search-order-with-parameter (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (replace-top-of-search-order word-lists (parameters-p1 parameters))))


;;; Structure (BEGIN-STRUCTURE) helpers

(defstruct (forth-structure (:conc-name #:fs-))
  (size 0)
  (word nil)
  (named? nil)
  (fields nil))

(defun add-structure-field (fs name field-size &optional (align? t))
  (with-forth-system (fs)
    (let* ((original-offset (stack-pop data-stack))
           (offset (cond ((zerop (mod original-offset (max field-size 1)))
                          original-offset)
                         (align?
                          (+ original-offset (- field-size (mod original-offset field-size))))
                         (t
                          original-offset)))
           (struct (stack-cell data-stack 0))
           (name (if (fs-named? struct)
                     (format nil "~A.~A" (word-name (fs-word struct)) name)
                     name))
           (word (make-word name #'push-field-address-from-parameter :smudge? t :parameters (make-parameters offset))))
      (push word (fs-fields struct))
      (add-and-register-word fs word)
      (stack-push data-stack (+ offset field-size)))))

(defun push-structure-size-from-parameter (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push data-stack (fs-size (parameters-p1 parameters)))))

(defun push-field-address-from-parameter (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push data-stack (+ (stack-pop data-stack) (parameters-p1 parameters)))))


;;; Helpers for FFI words

(defun push-parameter-as-global-pointer (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (let* ((name (parameters-p1 parameters))
           (forth-name (parameters-p2 parameters))
           (library (library-ffi-library (parameters-p3 parameters)))
           (pointer (cffi:foreign-symbol-pointer name :library library)))
      (if (null pointer)
          (forth-exception :undefined-foreign-global "Foreign global ~A~@[ (AS ~A)~] is not defined~@[ ~A~]"
                           name forth-name #+LispWorks (library-name (ffi-current-library ffi)) #-LispWorks nil)
          (stack-push data-stack (native-address memory pointer))))))

(defun push-parameter-as-callback-ptr (fs parameters)
  (declare (type forth-system fs) (type parameters parameters)
           (optimize (speed 3) (safety 0)))
  (with-forth-system (fs)
    (stack-push data-stack (native-address memory (cffi:get-callback (parameters-p1 parameters))))))
