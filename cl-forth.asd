(in-package #:asdf/user)

(defsystem #:cl-forth
  :long-name "CL-Forth"
  :description "Forth interpreter"
  :version (:read-file-line "version.text")
  :serial t
  :components ((:file "packages")
               (:file "exceptions")
               (:file "strings")
               (:file "numbers")
               (:file "memory")
               (:file "stacks")
               (:file "words")
               (:file "files")
               (:file "execution-tokens")
               (:file "system")
               (:file "helpers")
               (:module "forth-words"
                  :serial nil
                  :components ((:file "core")
                               (:file "environment")
                               (:file "files")
                               (:file "tools")
                               (:file "double")
                               (:file "float")
                               (:file "strings")
                               (:file "exceptions")
                               (:file "search")
                               (:file "facility")
                               #+ignore (:file "memory")
                               (:file "extensions")))
               (:file "asdf-support")
               ;;---*** TODO: Temporary
               (:file "test"))
  :in-order-to ((test-op (test-op #:cl-forth/test))))

(defsystem #:cl-forth/application
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
                     (symbol-call '#:forth '#:toplevel
                                  fs :evaluate "WARNING OFF S\" runtests.fth\" INCLUDED BYE")))))))
