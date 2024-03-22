(in-package #:cl-user)

(defpackage #:forth
  (:use #:common-lisp #:ccl)
  (:shadow #:toplevel)
  (:export #:forth-system
           #:toplevel))

(defpackage #:forth-words
  (:use))
