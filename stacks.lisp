(in-package #:forth)

(defstruct (stack (:constructor %make-stack) (:print-function %print-stack))
  name
  (cells #() :type array)
  (size 0 :type fixnum)
  (depth 0 :type fixnum)
  overflow-key
  underflow-key
  saved-cells
  (saved-depth 0))

(defun %print-stack (st stream depth)
  (declare (ignore depth))
  (print-unreadable-object (st stream :type t :identity t)
    (format stream "~A (~D/~D cell~:P)" (stack-name st) (stack-depth st) (stack-size st))))

(defun make-stack (name size overflow-key underflow-key)
  (%make-stack :name name :cells (make-array size :initial-element 0) :size size
               :overflow-key overflow-key :underflow-key underflow-key))

(defmacro define-stack-fun (name arglist &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,@arglist)
       (declare (optimize (speed 3) (safety 0)))
       ,@body)))

(define-stack-fun stack-overflow-check (st &optional (minimum-space 1))
  (declare (fixnum minimum-space))
  (unless (< (stack-depth st) (the fixnum (- (stack-size st) minimum-space -1)))
    (forth-exception (stack-overflow-key st))))

(define-stack-fun stack-underflow-check (st &optional (minimum-depth 1))
  (declare (fixnum minimum-depth))
  (when (< (stack-depth st) minimum-depth)
    (forth-exception (stack-underflow-key st))))

(define-stack-fun stack-cell (st index)
  ;; INDEX is zero-based index of element from top of stack
  (declare (fixnum index))
  (aref (stack-cells st) (- (stack-depth st) index 1)))

(define-stack-fun set-stack-cell (st index value)
  (declare (fixnum index))
  (setf (aref (stack-cells st) (- (stack-depth st) index 1)) value))

(defsetf stack-cell set-stack-cell)

(define-stack-fun stack-reset (st)
  (setf (stack-depth st) 0))

(define-stack-fun stack-push (st value)
  (stack-overflow-check st)
  (prog1
      (setf (aref (stack-cells st) (stack-depth st)) value)
    (incf (stack-depth st))))

(define-stack-fun stack-push-double (st value)
  (stack-overflow-check st 2)
  (multiple-value-bind (low high)
      (double-components value)
    (setf (aref (stack-cells st) (stack-depth st)) low)
    (setf (aref (stack-cells st) (1+ (stack-depth st))) high)
    (incf (stack-depth st) 2)
    value))

(define-stack-fun stack-pop (st)
  (stack-underflow-check st)
  (prog1
      (stack-cell st 0)
    (decf (stack-depth st))))

(define-stack-fun stack-pop-double (st)
  (stack-underflow-check st 2)
  (prog1
      (double-cell-signed (stack-cell st 1) (stack-cell st 0))
    (decf (stack-depth st) 2)))

(define-stack-fun stack-pop-double-unsigned (st)
  (stack-underflow-check st 2)
  (prog1
      (double-cell-unsigned (stack-cell st 1) (stack-cell st 0))
    (decf (stack-depth st) 2)))

(define-stack-fun stack-drop (st &optional (n 1))
  "( x(n) ... x(1) - )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (decf (stack-depth st) n))

(define-stack-fun stack-dup (st)
  "( x - x x )"
  (stack-underflow-check st)
  (stack-push st (stack-cell st 0)))

(define-stack-fun stack-?dup (st)
  "( x - 0 | x x )"
  (stack-underflow-check st)
  (unless (zerop (stack-cell st 0))
    (stack-push st (stack-cell st 0))))

(define-stack-fun stack-nip (st)
  "( x1 x2 - x2 )"
  (stack-underflow-check st 2)
  (prog1
      (setf (stack-cell st 1) (stack-cell st 0))
    (decf (stack-depth st))))

(define-stack-fun stack-over (st)
  "( x1 x2 - x1 x2 x1 )"
  (stack-underflow-check st 2)
  (stack-push st (stack-cell st 1)))

(define-stack-fun stack-pick (st n)
  "( +n - x )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (stack-push st (stack-cell st n)))

(define-stack-fun stack-roll (st n)
  "( x(n-1) xn x(n+1) ... x0 - x(n-1) x(n+1) ... x0 xn )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (let ((cell (stack-cell st n)))
    (loop for i downfrom (1- n) to 0
          do (setf (stack-cell st (1+ i)) (stack-cell st i)))
    (setf (stack-cell st 0) cell)))

(define-stack-fun stack-rot (st)
  "( x1 x2 x3 - x2 x3 x1 )"
  (stack-underflow-check st 3)
  (shiftf (stack-cell st 2) (stack-cell st 1) (stack-cell st 0) (stack-cell st 2)))
  
(define-stack-fun stack-swap (st)
  "( x1 x2 - x2 x1 )"
  (stack-underflow-check st 2)
  (shiftf (stack-cell st 1) (stack-cell st 0) (stack-cell st 1)))

(define-stack-fun stack-tuck (st)
  "( x1 x2 - x2 x1 x2 )"
  (stack-underflow-check st 2)
  (stack-push st (stack-cell st 0))
  (shiftf (stack-cell st 2) (stack-cell st 1) (stack-cell st 2)))

(define-stack-fun stack-2drop (st)
  "( x1 x2 - )"
  (stack-underflow-check st 2)
  (decf (stack-depth st) 2))

(define-stack-fun stack-2dup (st)
  "( x1 x2 - x1 x2 x1 x2 )"
  (stack-underflow-check st 2)
  (let ((x1 (stack-cell st 1))
        (x2 (stack-cell st 0)))
    (stack-push st x1)
    (stack-push st x2)))

(define-stack-fun stack-2over (st)
  "( x1 x2 x3 x4 - x1 x2 x3 x4 x1 x2 )"
  (stack-underflow-check st 4)
  (let ((x1 (stack-cell st 3))
        (x2 (stack-cell st 2)))
    (stack-push st x1)
    (stack-push st x2)))

(define-stack-fun stack-2rot (st)
  "( x1 x2 x3 x4 x5 x6 - x3 x4 x5 x6 x1 x2 )"
  (stack-underflow-check st 6)
  (shiftf (stack-cell st 4) (stack-cell st 2) (stack-cell st 0) (stack-cell st 4))
  (shiftf (stack-cell st 5) (stack-cell st 3) (stack-cell st 1) (stack-cell st 5)))

(define-stack-fun stack-2swap (st)
  "( x1 x2 x3 x4 - x3 x4 x1 x2 )"
  (stack-underflow-check st 4)
  (shiftf (stack-cell st 2) (stack-cell st 0) (stack-cell st 2))
  (shiftf (stack-cell st 1) (stack-cell st 3) (stack-cell st 1)))

;;; Control flow stack manipulation

(define-stack-fun stack-find-if (predicate st)
  (loop for n below (stack-depth st)
        when (funcall predicate (stack-cell st n))
          return n))

(define-stack-fun stack-snip (st n)
  "( x(n-1) xn x(n+1) ... x0 - x(n-1) x(n+1) ... x0 )"
  (declare (fixnum n))
  (stack-underflow-check st n)
  (prog1
      (let ((cell (stack-cell st n)))
        (loop for i downfrom (1- n) to 0
              do (setf (stack-cell st (1+ i)) (stack-cell st i)))
        cell)
    (decf (stack-depth st))))

;;; Save/Restore stack contents for FFI callbacks

(define-stack-fun save-stack (st)
  (setf (stack-saved-cells st) (copy-seq (stack-cells st))
        (stack-saved-depth st) (stack-depth st)
        (stack-depth st) 0))

(define-stack-fun restore-stack (st)
  (setf (stack-cells st) (copy-seq (stack-saved-cells st))
        (stack-depth st) (stack-saved-depth st)
        (stack-saved-cells st) nil))

;;; Display stack contents

(define-stack-fun show-stack (st base)
  (let ((depth (stack-depth st)))
    (declare (fixnum depth))
    (if (zerop depth)
        (format t "~&~A stack empty~%" (stack-name st))
        (progn
          (format t "~&Contents of ~A stack:~%" (string-downcase (stack-name st)))
          (dotimes (i depth)
            (format t "~2D: ~VR~%" i base (aref (stack-cells st) (- depth i 1))))))))
