(in-package #:forth)

(defclass stack ()
  ((cells :accessor stack-cells)
   (underflow-key :accessor stack-underflow-key :initarg :underflow-key :initform nil)
   (overflow-key :accessor stack-overflow-key :initarg :overflow-key :initform nil)))

(defmethod initialize-instance :after ((st stack) &key initial-size &allow-other-keys)
  (with-slots (cells underflow-key overflow-key) st
    (setf cells (make-array initial-size :adjustable t :fill-pointer 0))
    (assert (keywordp underflow-key) ((stack-underflow-key st)) "Value of ~S must be a keyword" :underflow-key)
    (assert (keywordp overflow-key) ((stack-overflow-key st)) "Value of ~S must be a keyword" :overflow-key)))

(declaim (inline stack-underflow-check))
(defun stack-underflow-check (st &optional (minimum-depth 1))
  (with-slots (cells underflow-key) st
    (when (< (fill-pointer cells) minimum-depth)
      (forth-exception underflow-key))))
  
(declaim (inline stack-cell))
;;; INDEX is zero-based index of element from top of stack
(defun stack-cell (st index)
  (with-slots (cells) st
    (aref cells (- (fill-pointer cells) index 1))))

(declaim (inline set-stack-cell))
(defun set-stack-cell (st index value)
  (with-slots (cells) st
    (setf (aref cells (- (fill-pointer cells) index 1)) value)))

(defsetf stack-cell set-stack-cell)

(defmethod stack-reset ((st stack))
  (setf (fill-pointer (stack-cells st)) 0))

(defmethod stack-push ((st stack) value)
  (vector-push-extend value (stack-cells st)))

(defmethod stack-push-double ((st stack) value)
  (with-slots (cells) st
    (multiple-value-bind (low high)
        (double-components value)
      (vector-push-extend high cells)
      (vector-push-extend low cells))))

(defmethod stack-pop ((st stack))
  (stack-underflow-check st)
  (prog1
      (stack-cell st 0)
    (decf (fill-pointer (stack-cells st)))))

(defmethod stack-pop-double ((st stack))
  (stack-underflow-check st 2)
  (prog1
      (double-cell-signed (stack-cell st 0) (stack-cell st 1))
    (decf (fill-pointer (stack-cells st)) 2)))

(defmethod stack-pop-double-unsigned ((st stack))
  (stack-underflow-check st 2)
  (prog1
      (double-cell-unsigned (stack-cell st 0) (stack-cell st 1))
    (decf (fill-pointer (stack-cells st)) 2)))

(defmethod stack-depth ((st stack))
  (fill-pointer (stack-cells st)))

(defmethod stack-drop ((st stack))
  "( x - )"
  (stack-underflow-check st)
  (decf (fill-pointer (stack-cells st))))

(defmethod stack-dup ((st stack))
  "( x - x x )"
  (stack-underflow-check st)
  (stack-push st (stack-cell st 0)))

(defmethod stack-?dup ((st stack))
  "( x - 0 | x x )"
  (stack-underflow-check st)
  (unless (zerop (stack-cell st 0))
    (stack-push st (stack-cell st 0))))

(defmethod stack-nip ((st stack))
  "( x1 x2 - x2 )"
  (stack-underflow-check st 2)
  (prog1
      (setf (stack-cell st 1) (stack-cell st 0))
    (decf (fill-pointer (stack-cells st)))))

(defmethod stack-over ((st stack))
  "( x1 x2 - x1 x2 x1 )"
  (stack-underflow-check st 2)
  (stack-push st (stack-cell st 1)))

(defmethod stack-pick ((st stack) n)
  "( +n - x )"
  (stack-underflow-check st n)
  (stack-push st (stack-cell st n)))

(defmethod stack-rot ((st stack))
  "( x1 x2 x3 - x2 x3 x1 )"
  (stack-underflow-check st 3)
  (shiftf (stack-cell st 2) (stack-cell st 1) (stack-cell st 0) (stack-cell st 2)))
  
(defmethod stack-swap ((st stack))
  "( x1 x2 - x2 x1 )"
  (stack-underflow-check st 2)
  (shiftf (stack-cell st 1) (stack-cell st 0) (stack-cell st 1)))

(defmethod stack-tuck ((st stack))
  "( x1 x2 - x2 x1 x2 )"
  (stack-underflow-check st 2)
  (stack-push st (stack-cell st 0))
  (shiftf (stack-cell st 2) (stack-cell st 1) (stack-cell st 2)))

(defmethod stack-2drop ((st stack))
  "( x1 x2 - )"
  (stack-underflow-check st 2)
  (decf (fill-pointer (stack-cells st)) 2))

(defmethod stack-2dup ((st stack))
  "( x1 x2 - x1 x2 x1 x2 )"
  (stack-underflow-check st 2)
  (let ((x1 (stack-cell st 1))
        (x2 (stack-cell st 0)))
    (stack-push st x1)
    (stack-push st x2)))

(defmethod stack-2over ((st stack))
  "( x1 x2 x3 x4 - x1 x2 x3 x4 x1 x2 )"
  (stack-underflow-check st 4)
  (let ((x1 (stack-cell st 3))
        (x2 (stack-cell st 2)))
    (stack-push st x1)
    (stack-push st x2)))

(defmethod stack-2rot ((st stack))
  "( x1 x2 x3 x4 x5 x6 - x3 x4 x5 x6 x1 x2 )"
  (stack-underflow-check st 6)
  (shiftf (stack-cell st 4) (stack-cell st 2) (stack-cell st 0) (stack-cell st 4))
  (shiftf (stack-cell st 5) (stack-cell st 3) (stack-cell st 1) (stack-cell st 5)))

(defmethod stack-2swap ((st stack))
  "( x1 x2 x3 x4 - x3 x4 x1 x2 )"
  (stack-underflow-check st 4)
  (shiftf (stack-cell st 2) (stack-cell st 0) (stack-cell st 2))
  (shiftf (stack-cell st 1) (stack-cell st 3) (stack-cell st 1)))

;;; Control flow stack manipulation

(defmethod stack-find-if (predicate (st stack))
  (loop for n below (stack-depth st)
        when (funcall predicate (stack-cell st n))
          return n))

(defmethod stack-snip ((st stack) n)
  "( x(n-1) xn x(n+1) ... x0 - x(n-1) x(n+1) ... x0 )"
  (stack-underflow-check st n)
  (prog1
      (let ((cell (stack-cell st n)))
        (loop for i downfrom (1- n) to 0
              do (setf (stack-cell st (1+ i)) (stack-cell st i)))
        cell)
    (decf (fill-pointer (stack-cells st)))))
