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

(defclass ffi ()
  ((foreign-space :accessor ffi-foreign-space
                  :initform (make-instance 'data-space :size +cell-size+)))
  )

(defmethod save-to-template ((ffi ffi))
  nil)

(defmethod load-from-template ((ffi ffi) template fs)
  (declare (ignore template fs))
  nil)
