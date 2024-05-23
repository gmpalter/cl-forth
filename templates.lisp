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
            (template-exception-hook template) exception-hook
            (template-exception-prefix template) exception-prefix
            (template-exit-hook template) exit-hook
            (template-announce-addendum template) announce-addendum
            (template-prompt-string template) prompt-string))
    template))

(define-forth-method load-from-template (fs template)
  (load-from-template memory (template-memory template))
  (load-from-template word-lists (template-word-lists template))
  (load-from-template execution-tokens (template-execution-tokens template))
  (load-from-template ffi (template-ffi template))
  (load-from-template replacements (template-replacements template))
  (setf base (template-base template)
        float-precision (template-float-precision template)
        show-redefinition-warnings? (template-show-redefinition-warnings? template)
        show-definition-code? (template-show-definition-code? template)
        exception-hook (template-exception-hook template)
        exception-prefix (template-exception-prefix template)
        exit-hook (template-exit-hook template)
        announce-addendum (template-announce-addendum template)
        prompt-string (template-prompt-string template))
  (register-predefined-words word-lists execution-tokens (data-space-high-water-mark memory)))

(defun load-forth-system-from-template (template)
  ;; No sense in installing the predefined words as we're going to erase them when we reload the template's word lists
  (clrhash *predefined-words*)
  (let ((fs (make-instance 'forth-system)))
    (load-from-template fs template)
    fs))