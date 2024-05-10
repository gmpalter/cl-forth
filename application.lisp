;;; -*- Syntax: Common-Lisp; Base: 10 -*-

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
                #:shared-resource-primary-owner)
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
	   :option-char #\e
	   :long-name "eval"
	   :keyword :eval
	   :help-string "Evaluate <form> (may need to quote <form> in shell)"
	   :may-take-operand t
	   :allow-multiple t)
          (make-command-line-argument
           :long-name "trace"
           :keyword :trace
           :help-string "Record all input and output to a file"
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
                                            (and (member (car x) '(:eval :trace)) (list x)))
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
          (let ((evaluate nil)
                (trace nil))
            (dolist (arg processed-arguments)
              (when (eq (car arg) ':eval)
                (push (cdr arg) evaluate))
              (when (eq (car arg) ':trace)
                (setf trace (cdr arg))))
            (let ((clean-exit? (forth:run :asdf-system asdf-system :template template :evaluate evaluate :trace trace)))
              (if clean-exit?
                  (quit)
                  (quit -1)))))))
  ;; Ensure that CCL's housekeeping functions run periodcally as intended
  (%set-toplevel (lambda ()
                   (with-standard-initial-bindings
                     (housekeeping-loop))))
  (toplevel))
