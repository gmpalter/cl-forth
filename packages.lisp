(in-package #:cl-user)

(defpackage #:forth
  (:use #:common-lisp)
  #+CCL (:import-from #:ccl
                      #:fixnump
                      #:whitespacep
                      #:class-direct-slots
                      #:slot-definition-name)
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
  (:export #:forth-system
           #:forth-toplevel
           #:*exception-hook*
           #:*exception-prefix*
           #:*exit-hook*))

(defpackage #:forth-words
  (:use))
