(in-package #:asdf/user)

(defsystem #:cl-forth
  :description "Forth interpreter"
  :version "0.9"
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
                               #+ignore (:file "memory")
                               (:file "files")
                               (:file "tools")
                               (:file "double")
                               #+ignore (:file "float")
                               (:file "strings")
                               (:file "exceptions")
                               (:file "search")
                               #+ignore (:file "facility")
                               (:file "extensions")))
               ;;---*** TODO: Temporary
               (:file "test")))

(defsystem #:cl-forth/application
  )

(defsystem #:cl-forth/test
  )
