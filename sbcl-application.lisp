;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:forth-application)

(defparameter +forth-help-message+
  "Usage: ~A <options>
	 where <options> are one or more of:
        -h, --help : print this text and exit
        -V, --version : print version information and exit
        -i, --interpret : Pass <text> to the Forth interpreter (may need to quote <text> in shell)
            --transcript : Create a timestamped transcript of this session in <path>")

(defclass forth-application ()
  ((asdf-system :initarg :asdf-system :initform '#:cl-forth)
   (template :initarg :template :initform nil))
  )

(defun toplevel (app)
  (with-slots (asdf-system template) app
    (let ((options (rest *posix-argv*))
          (interpret nil)
          (transcript nil))
      (loop while options do
        (labels ((option-error (control-string &rest arguments)
                   (format *error-output* "~&~?~%" control-string arguments)
                   (exit :code -1))
                 (pop-option (&optional following)
                   (cond (options
                          (pop options))
                         (following
                          (option-error "Value missing after \"~A\"" following))
                         (t
                          (option-error "Missing command line arguments")))))
          (let ((option (first options)))
            (cond ((or (string= option "--interpret") (string= option "-i"))
                   (pop-option)
                   (push (pop-option option) interpret))
                  ((string= option "--transcript")
                   (pop-option)
                   (if transcript
                       (option-error "Multiple --transcript options")
                       (setf transcript (pop-option "--transcript"))))
                  ((or (string= option "--help") (string= option "-h"))
                   (fresh-line)
                   (format t +forth-help-message+ (first *posix-argv*))
                   (terpri)
                   (exit))
                  ((or (string= option "--version") (string= option "-V"))
                   (let ((fs (make-instance 'forth:forth-system :template template)))
                     (forth:announce-forth fs asdf-system))
                   (exit))
                  (t
                   (option-error "Unknown option: \"~A\"" option))))))
      (let ((clean-exit? (forth:run :asdf-system asdf-system :template template
                                    :interpret interpret :transcript-file transcript)))
        (exit :code (if clean-exit? 0 -1))))))

(defun save-application (filename &key (application-class 'forth-application))
  (let ((app (make-instance application-class)))
    (save-lisp-and-die filename :toplevel #'(lambda () (toplevel app)) :executable t :save-runtime-options t)))

