(in-package #:cl-user)

(defpackage #:forth
  (:use #:common-lisp #:ccl)
  (:shadow #:toplevel)
  (:export #:forth-system
           #:toplevel
           #:*exception-hook*
           #:*exception-prefix*
           #:*exit-hook*))

(defpackage #:forth-words
  (:use))
