;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://github.com/gmpalter/cl-forth/tree/main?tab=MIT-1-ov-file#readme

(in-package #:cl-user)

(defpackage #:forth-application
  (:nicknames #:forth-app)
  (:use #:ccl #:common-lisp)
  (:import-from #:ccl
                #:application
                #:application-version-string
                #:*standard-help-argument*
	        #:*standard-version-argument*
                #:*standard-terminal-encoding-argument*
                #:make-command-line-argument
                #:command-line-arguments
                #:process-application-arguments
                #:%usage-exit
                #:summarize-option-syntax
                #:with-standard-initial-bindings
                #:housekeeping-loop
                #:make-application-error-handler
                #:*terminal-input*
                #:input-stream-shared-resource
                #:shared-resource-primary-owner
                #:*terminal-output*)
  (:export #:forth-application))

(in-package #:forth-application)

(defclass forth-application (application)
  ((asdf-system :initarg :asdf-system :initform '#:cl-forth)
   (template :initarg :template :initform nil)
   (command-line-arguments
    :initform
    (list *standard-help-argument*
	  *standard-version-argument*
          *standard-terminal-encoding-argument*
	  (make-command-line-argument
	   :option-char #\i
	   :long-name "interpret"
	   :keyword :interpret
	   :help-string "Pass <text> to the Forth interpreter (may need to quote <text> in shell)"
	   :may-take-operand t
	   :allow-multiple t)
          (make-command-line-argument
           :long-name "transcript"
           :keyword :transcript
           :help-string "Create a timestamped transcript of this session in <path>"
           :may-take-operand t
           :allow-multiple nil)))
   (processed-arguments :initform nil))
  )

(defmethod application-version-string ((app forth-application))
  (with-slots (asdf-system) app
    (let ((me (asdf:find-system asdf-system)))
      (format nil "~A version ~A~%Running under ~A" (asdf:system-long-name me) (asdf:component-version me)
              (call-next-method)))))

(defmethod process-application-arguments ((app forth-application) error-flag options args)
  (declare (ignorable error-flag))
  (call-next-method)			; handle help, errors
  (if args
    (%usage-exit (format nil "Unrecognized non-option arguments: ~a" args)
		 #-windows-target #-android-target #$EX_USAGE #+android-target 64 #+windows-target #$EXIT_FAILURE
		 (summarize-option-syntax app))
    (with-slots (processed-arguments) app
      (setf processed-arguments (mapcan #'(lambda (x)
                                            (and (member (car x) '(:interpret :transcript)) (list x)))
                                        options)))))

(defmethod toplevel-function ((app forth-application) init-file)
  (declare (ignore init-file))
  (ccl:process-run-function "Forth"
    #'(lambda ()
        (with-slots (asdf-system template processed-arguments) app
          (make-application-error-handler app :quit)
          (let ((sr (input-stream-shared-resource *terminal-input*)))
            (when sr
              (setf (shared-resource-primary-owner sr) *current-process*)))
          (let ((interpret nil)
                (transcript-file nil))
            (dolist (arg processed-arguments)
              (when (eq (car arg) ':interpret)
                (push (cdr arg) interpret))
              (when (eq (car arg) ':transcript)
                (setf transcript-file (cdr arg))))
            (add-auto-flush-stream *terminal-output*)
            (let ((clean-exit? (forth:run :asdf-system asdf-system :template template
                                          :interpret interpret :transcript-file transcript-file)))
              (if clean-exit?
                  (quit)
                  (quit -1)))))))
  ;; Ensure that CCL's housekeeping functions run periodcally as intended
  (%set-toplevel (lambda ()
                   (with-standard-initial-bindings
                     (housekeeping-loop))))
  (toplevel))
