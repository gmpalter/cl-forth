(in-package #:forth)

;;; Exception Word Set as defined in Section 9 of the Forth 2012 specification

(define-word catch (:word "CATCH")
  (let ((token (stack-pop data-stack)))
    (stack-push exception-stack (make-exception-frame fs))
    (handler-case
        (progn
          (execute execution-tokens token fs)
          (stack-push data-stack 0)
          (stack-pop exception-stack))
      (forth-exception (fe)
        (let ((exception-frame (stack-pop exception-stack)))
          (apply-exception-frame fs exception-frame))
        (stack-push data-stack (forth-exception-code fe))))))

(define-word throw (:word "THROW")
  (let ((n (cell-signed (stack-pop data-stack))))
    (unless (zerop n)
      (forth-exception-by-code n))))
