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

(defclass template ()
  ((memory :accessor template-memory)
   (word-lists :accessor template-word-lists)
   (execution-tokens :accessor template-execution-tokens)
   (ffi :accessor template-ffi)
   (replacements :accessor template-replacements)
   (base :accessor template-base)
   (float-precision :accessor template-float-precision)
   (show-redefinition-warnings? :accessor template-show-redefinition-warnings?)
   (show-definition-code? :accessor template-show-definition-code?)
   (%optimize-definitions? :accessor template-%optimize-definitions?)
   (show-backtraces-on-error? :accessor template-show-backtraces-on-error?)
   (exception-hook :accessor template-exception-hook)
   (exception-prefix :accessor template-exception-prefix)
   (exit-hook :accessor template-exit-hook)
   (announce-addendum :accessor template-announce-addendum)
   (prompt-string :accessor template-prompt-string))
  )

(defun save-forth-system-to-template (fs)
  (let ((template (make-instance 'template)))
    (with-forth-system (fs)
      (setf (template-memory template) (save-to-template memory)
            (template-word-lists template) (save-to-template word-lists)
            (template-execution-tokens template) (save-to-template execution-tokens)
            (template-ffi template) (save-to-template ffi)
            (template-replacements template) (save-to-template replacements)
            (template-base template) base
            (template-float-precision template) float-precision
            (template-show-redefinition-warnings? template) show-redefinition-warnings?
            (template-show-definition-code? template) show-definition-code?
            (template-%optimize-definitions? template) %optimize-definitions?
            (template-show-backtraces-on-error? template) show-backtraces-on-error?
            (template-exception-hook template) exception-hook
            (template-exception-prefix template) exception-prefix
            (template-exit-hook template) exit-hook
            (template-announce-addendum template) announce-addendum
            (template-prompt-string template) prompt-string))
    template))

(defmethod load-from-template ((fs forth-system) template fsx)
  (declare (ignore fsx))
  (with-forth-system (fs)
    (load-from-template memory (template-memory template) fs)
    (load-from-template word-lists (template-word-lists template) fs)
    (load-from-template execution-tokens (template-execution-tokens template) fs)
    (load-from-template ffi (template-ffi template) fs)
    (load-from-template replacements (template-replacements template) fs)
    (setf base (template-base template)
          float-precision (template-float-precision template)
          show-redefinition-warnings? (template-show-redefinition-warnings? template)
          show-definition-code? (template-show-definition-code? template)
          %optimize-definitions? (template-%optimize-definitions? template)
          show-backtraces-on-error? (template-show-backtraces-on-error? template)
          exception-hook (template-exception-hook template)
          exception-prefix (template-exception-prefix template)
          exit-hook (template-exit-hook template)
          announce-addendum (template-announce-addendum template)
          prompt-string (template-prompt-string template))
    (register-predefined-words word-lists execution-tokens (data-space-high-water-mark memory))))

(defun load-forth-system-from-template (template)
  ;; No sense in installing the predefined words as we're going to erase them when we reload the template's word lists
  (clrhash *predefined-words*)
  (let ((fs (make-instance 'forth-system)))
    (load-from-template fs template fs)
    fs))
