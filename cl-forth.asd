;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:asdf/user)

(defsystem #:cl-forth
  :long-name "CL-Forth"
  :description "Forth interpreter"
  :version (:read-file-line "version.text")
  :serial t
  :depends-on (#:cffi)
  :components ((:file "packages")
               (:file "in-memory-streams" :if-feature :ccl)
               (:file "compatibility")
               (:file "exceptions")
               (:file "strings")
               (:file "numbers")
               (:file "memory")
               (:file "stacks")
               (:file "words")
               (:file "files")
               (:file "execution-tokens")
               (:file "ffi")
               (:file "system")
               (:file "templates")
               (:file "helpers")
               (:file "asdf-support")
               (:file "run")
               (:module "forth-words"
                  :serial nil
                  :components ((:file "core")
                               (:file "environment")
                               (:file "double")
                               (:file "exceptions")
                               (:file "facility")
                               (:file "files")
                               (:file "float")
                               (:file "locals")
                               (:file "memory")
                               (:file "tools")
                               (:file "search")
                               (:file "strings")
                               (:file "ffi")
                               ;; Useful words not defined in Forth 2012
                               (:file "extensions"))))
  :in-order-to ((test-op (test-op #:cl-forth/test))))

#+SBCL
;;; Suppress compiler notes until I have time to figure out how to resolve them
(defmethod perform :around ((o compile-op) (c cl-source-file))
  (handler-bind ((sb-ext:compiler-note #'(lambda (c)
                                           (declare (ignore c))
                                           (invoke-restart 'muffle-warning))))
    (call-next-method)))

#+CCL
(defsystem #:cl-forth/application
  :long-name "CL-Forth App"
  :description "CL-Forth standalone application"
  :version (:read-file-line "version.text")
  :depends-on (#:cl-forth)
  :serial t
  :components ((:file "application"))
  )

(defsystem #:cl-forth/test
  :description "Test Forth interpreter"
  :pathname "tests/src"
  :perform (test-op (o c)
             (let ((tests-dir (component-pathname c)))
               (uiop:with-current-directory (tests-dir)
                 (with-input-from-string (text #.(format nil "The quick brown fox jumped over the lazy red dog.~%"))
                   ;; Allow input from the console if any test raises a Forth exception
                   (let ((*standard-input* (make-concatenated-stream text *standard-input*))
                         (fs (make-instance (find-symbol* '#:forth-system '#:forth))))
                     (symbol-call '#:forth '#:forth-toplevel
                                  fs :interpret "WARNING OFF S\" runtests.fth\" INCLUDED BYE")))))))
