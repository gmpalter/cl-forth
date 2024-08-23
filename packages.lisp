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

#+CCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:prefixed-stream)
  (require '#:timestamped-stream))

(defpackage #:forth
  (:use #:common-lisp)
  #+CCL (:import-from #:ccl
                      #:fixnump
                      #:whitespacep
                      #:class-direct-slots
                      #:slot-definition-name
                      #:process-run-function
                      #:process-wait
                      #:add-auto-flush-stream
                      #:remove-auto-flush-stream
                      #:make-prefixed-stream
                      #:make-timestamped-stream)
  #+SBCL (:import-from #:sb-int
                       #:fixnump
                       #:named-lambda)
  #+SBCL (:import-from #:sb-mop
                       #:class-direct-slots
                       #:slot-definition-name)
  #+LispWorks (:import-from #:lispworks
                            #:fixnump)
  #+LispWorks (:import-from #:harlequin-common-lisp
                            #:class-direct-slots
                            #:slot-definition-name)
  #+LispWorks (:import-from #:mp
                            #:process-wait)
  (:export #:forth-system
           #:forth-toplevel
           #:announce-forth
           #:save-forth-system-to-template
           #:load-forth-system-from-template
           #:*exception-hook*
           #:*exception-prefix*
           #:*exit-hook*
           #:run-forth-process
           #:run))

(defpackage #:forth-words
  (:use))

(defpackage #:forth-ffi-symbols
  (:use))

(defpackage #:forth-application
  (:nicknames #:forth-app)
  (:use #:common-lisp)
  #+CCL (:import-from #:ccl
                      #:application
                      #:application-version-string
                      #:*standard-help-argument*
	              #:*standard-version-argument*
                      #:*standard-terminal-encoding-argument*
                      #:make-command-line-argument
                      #:command-line-arguments
                      #:process-application-arguments
                      #:toplevel-function
                      #:%usage-exit
                      #:summarize-option-syntax
                      #:with-standard-initial-bindings
                      #:housekeeping-loop
                      #:make-application-error-handler
                      #:*terminal-input*
                      #:input-stream-shared-resource
                      #:shared-resource-primary-owner
                      #:*terminal-output*
                      #:add-auto-flush-stream
                      #:*initial-process*
                      #:*current-process*
                      #:toplevel
                      #:%set-toplevel)
  #+SBCL (:import-from #:sb-ext
                       #:*posix-argv*
                       #:exit
                       #:save-lisp-and-die)
  (:export #:forth-application
           #:save-application))
