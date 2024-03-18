(in-package #:forth)

;;; Exception words as defined in Section 9 of the Forth 2012 specification

(define-word catch (:word "CATCH")
  "( i*x xt – j*x 0 | i*x n)"
  "Push an exception frame on the exception stack and then execute the execution token XT (as with EXECUTE) in such a way"
  "that control can be transferred to a point just after CATCH if THROW is executed during the execution of XT."
  "If the execution of XT completes normally (i.e., the exception frame pushed by this CATCH is not popped by an execution"
  "of THROW) pop the exception frame and return zero on top of the data stack, above whatever stack items would have been"
  "returned by XT EXECUTE. Otherwise, the remainder of the execution semantics are given by THROW"
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
  "( k*x n – k*x | i*x n)"
  "If any bits of N are non-zero, pop the topmost exception frame from the exception stack, along with everything on the"
  "return stack above that frame. Then restore the input source specification in use before the corresponding CATCH and"
  "adjust the depths of all stacks defined by this standard so that they are the same as the depths saved in the exception"
  "frame (I is the same number as the I in the input arguments to the corresponding CATCH), put N on top of the data stack,"
  "and transfer control to a point just after the CATCH that pushed that exception frame."
  "If the top of the stack is non zero and there is no exception frame on the exception stack, the behavior is as follows:"
  "If N is minus-one (-1), empty the data stack and perform the function of QUIT, which includes emptying the return stack"
  "If N is minus-two (-2), display the MESSAGE associated with the ABORT\" that generated the THROW, empty the data stack"
  "and perform the function of QUIT, which includes emptying the return stack"
  "Otherwise, the system may display an implementation-dependent message giving information about the condition associated"
  "with the THROW code N. Subsequently, the system shall empty the data stack and perform the function of QUIT, which includes"
  "emptying the return stack"
  (let ((n (cell-signed (stack-pop data-stack))))
    (unless (zerop n)
      (forth-exception-by-code n))))


;;; Exception extension words as defined in Section 9 of the Forth 2012 specification

(define-word abort (:word "ABORT")
  "(S: i*x - ) (R: j*x - )"
  "Perform the function of -1 THROW"
  (forth-exception :abort))

;;; Marked as IMMEDIATE so we can grab the message at compile-time and generate the correct code sequence
(define-word abort-with-message (:word "ABORT\"" :immediate? t :compile-only? t)
  "ABORT\" <message>\""
  "(S: i*x x1 - | i*x ) (R: j*x - | j*x )"
  "Remove X1 from the stack. If any bit of X1 is not zero, perform the function of -2 THROW,"
  "displaying MESSAGE if there is no exception frame on the exception stack"
  (let ((message (parse files #\")))
    (add-to-definition fs
      `(when (truep (stack-pop data-stack))
         (forth-exception :abort\" "~@[In ~A: ~]~A" ,(word-name (definition-word definition)) ,message)))))
