;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:cl-user)

(defpackage #:forth-asdf-support
  (:use #:common-lisp #:forth #:asdf)
  (:import-from #:forth
                #:make-forth-system
                #:fs-files
                #:+read-direction+
                #:source-push
                #:forth-open-file
                #:interpreter/compiler
                #:reset-interpreter/compiler
                #:forth-exception
                #:forth-exception-key
                #:forth-exception-phrase
                #:show-backtrace
                #:current-input-state)
  (:export #:forth-asdf-system
           #:forth-source-file
           #:system-forth-template))

(in-package #:forth-asdf-support)


;;; Define a FORTH-ASDF-SYSTEM

(defclass forth-asdf-system (system)
  ((forth-system :accessor fas-forth-system :initform (make-forth-system)))
  )

(defmethod initialize-instance :after ((fas forth-asdf-system) &key &allow-other-keys)
  (with-slots (forth-system) fas
    (reset-interpreter/compiler forth-system)))

(defmethod reinitialize-instance :after ((fas forth-asdf-system) &key &allow-other-keys)
  (with-slots (forth-system) fas
    (reset-interpreter/compiler forth-system)))


;;; Define the FORTH-SOURCE component type

(defclass forth-source-file (source-file)
  ((type :initform "4th")))

(defmethod perform ((o compile-op) (c forth-source-file))
  nil)

(defmethod perform ((o load-op) (c forth-source-file))
  (let* ((fs (or (loop for parent = (component-parent c) then (component-parent parent)
                         thereis (and (typep parent 'forth-asdf-system)
                                      (fas-forth-system parent)))
                 (make-forth-system)))
         (files (fs-files fs)))
    (source-push files :fileid (forth-open-file files (component-pathname c) +read-direction+))
    (handler-case
        (interpreter/compiler fs :toplevel? nil)
      (forth-exception (e)
        (write-line (forth-exception-phrase e))
        (show-backtrace fs)
        (let ((context-format "interpreting line ~D, character ~D: \"~A\"")
              (context-arguments (multiple-value-bind (buffer >in lineno)
                                     (current-input-state (fs-files fs))
                                   (list lineno >in buffer))))
          (case *compile-file-failure-behaviour*
            (:warn (warn 'compile-failed-warning
                         :description "Forth load failed"
                         :context-format context-format
                         :context-arguments context-arguments))
            (:error (error 'compile-failed-error
                           :description "Forth load failed"
                           :context-format context-format
                           :context-arguments context-arguments))
            (:ignore nil)))))))


;;; Make FORTH-ASDF-SYSTEM and FORTH-SOURCE-FILE available to DEFSYSTEM forms

(uiop:import* 'forth-asdf-support:forth-asdf-system '#:asdf)
(uiop:import* 'forth-asdf-support:forth-source-file '#:asdf)


;;; Create a template after loading a FORTH-ASDF-SYSTEM

(defun system-forth-template (system)
  (let* ((system (find-system system))
         (fs (fas-forth-system system)))
    (prog1
        (save-forth-system-to-template fs)
      (setf (fas-forth-system system) nil))))
