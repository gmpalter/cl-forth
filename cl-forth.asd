(in-package #:asdf/user)

(defsystem #:cl-forth
  :description "Forth interpreter"
  :version "0.9"
  :serial t
  :components ((:file "packages")
               (:file "exceptions")
               (:file "numbers")
               (:file "strings")
               (:file "memory")
               (:file "stacks")
               (:file "words")
               (:file "files")
               (:file "system")
               (:module "forth-words"
                  :serial nil
                  :components ((:file "core")
                               #+ignore (:file "memory")
                               #+ignore (:file "files")
                               (:file "tools")
                               (:file "double")
                               #+ignore (:file "float")
                               #+ignore (:file "strings")
                               #+ignore (:file "exceptions")
                               (:file "search")
                               #+ignore (:file "facility")
                               (:file "extensions")))
               ;;---*** TODO: Temporary
               (:file "test")))

(defsystem #:cl-forth/application
  )

(defsystem #:cl-forth/test
  )
