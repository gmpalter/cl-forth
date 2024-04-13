(in-package #:cl-user)

(defpackage #:forth
  (:use #:common-lisp)
  #+CCL (:import-from #:ccl
                      #:fixnump
                      #:whitespacep
                      #:class-direct-slots)
  #+SBCL (:import-from #:sb-int
                       #:fixnump)
  #+SBCL (:import-from #:sb-mop
                       #:class-direct-slots)
  #+LispWorks (:import-from #:lispworks
                            #:fixnump)
  #+LispWorks (:import-from #:harlequin-common-lisp
                            #:class-direct-slots)
  (:export #:forth-system
           #:forth-toplevel
           #:*exception-hook*
           #:*exception-prefix*
           #:*exit-hook*))

(defpackage #:forth-words
  (:use))
