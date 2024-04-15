(in-package #:forth)

#+CCL
(defmacro named-lambda (name arglist &body body)
  `(ccl:nfunction ,name (lambda ,arglist ,@body)))

#+LispWorks
(defmacro named-lambda (name arglist &body body)
  (declare (ignore name))
  `(lambda ,arglist ,@body))
