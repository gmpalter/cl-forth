(in-package #:forth)

(defconstant +true+ -1)
(defconstant +false+ 0)

(declaim (inline truep))
(defun truep (x) (not (zerop x)))

(declaim (inline falsep))
(defun falsep (x) (zerop x))

(defconstant +most-positive-single-cell+ (1- (dpb 1 (byte 1 63) 0)))
(defconstant +most-negative-single-cell+ (- (dpb 1 (byte 1 63) 0)))

(defconstant +most-positive-double-cell+ (1- (dpb 1 (byte 1 127) 0)))
(defconstant +most-negative-double-cell+ (- (dpb 1 (byte 1 127) 0)))

(defun interpret-number (thing base &key (allow-floats? nil) (signal-overflow? t))
  (flet ((interpret-base-prefix ()
           (let ((ch (aref thing 0)))
             (cond ((char-equal ch #\#) (values 10 1))
                   ((char-equal ch #\$) (values 16 1))
                   ((char-equal ch #\%) (values  2 1))
                   (t                 (values base 0))))))
    (cond ((and allow-floats?
                (= base 10)
                (let ((ch (aref thing 0)))
                  (or (digit-char-p ch)
                      (char-equal ch #\+)
                      (char-equal ch #\-)))
                (= (count #\E thing :test #'char-equal) 1))
           ;; When BASE is 10 and the string contains exactly one "E", try floating point
           (let* ((*read-default-float-format* 'double-float)
                  (thing (if (char-equal (aref thing (1- (length thing))) #\E)
                             (concatenate 'string thing "0")
                             thing))
                  (float (ccl::parse-float thing (length thing) 0)))
             (if float
                 (values :float float)
                 (values nil nil))))
          ;; 'c' is interpreted as a character literal
          ((and (= (length thing) 3)
                (char-equal (aref thing 0) #\')
                (char-equal (aref thing 2) #\'))
           (values :single (forth-char (aref thing 1))))
          ((and (= (count #\. thing :test #'char-equal) 1)
                (char-equal (aref thing (1- (length thing))) #\.))
           ;; If the string end with a period, try a double precision integer
           (handler-case
               (multiple-value-bind (base start)
                   (interpret-base-prefix)
                 (let ((value (parse-integer thing :radix base :start start :end (1- (length thing)))))
                   (cond ((<= +most-negative-double-cell+ value +most-positive-double-cell+)
                          (values :double value))
                         (signal-overflow?
                          (forth-exception :out-of-range "Value too large for a double integer"))
                         (t
                          (values nil nil)))))
             (parse-error ()
               (values nil nil))))
          (t
           ;; Otherwise, try a single precision integer
           (handler-case
               (multiple-value-bind (base start)
                   (interpret-base-prefix)
                 (let ((value (parse-integer thing :radix base :start start)))
                   (cond ((<= +most-negative-single-cell+ value +most-positive-single-cell+)
                          (values :single value))
                         (signal-overflow?
                          (forth-exception :out-of-range "Value too large for an integer"))
                         (t
                          (values nil nil)))))
             (parse-error ()
               (values nil nil)))))))

(declaim (inline cell-signed))
(defun cell-signed (cell)
  (cond ((fixnump cell) cell)
        ((<= +most-negative-single-cell+ cell +most-positive-single-cell+)
         cell)
        (t
         (let ((raw (ldb (byte 64 0) cell)))
           (if (zerop (ldb (byte 1 63) raw))
               raw
               (- raw (dpb 1 (byte 1 64) 0)))))))

(declaim (inline cell-unsigned))
(defun cell-unsigned (cell)
  (ldb (byte 64 0) cell))

(declaim (inline double-components))
(defun double-components (double)
  (values (ldb (byte 64 0) double) (ldb (byte 64 64) double)))

(declaim (inline double-cell-signed))
(defun double-cell-signed (low-cell high-cell)
  (let ((value (dpb high-cell (byte 64 64) (ldb (byte 64 0) low-cell))))
    (if (<= +most-negative-double-cell+ value +most-positive-double-cell+)
        value
        (let ((raw (ldb (byte 128 0) value)))
          (if (zerop (ldb (byte 1 127) raw))
              raw
              (- raw (dpb 1 (byte 1 128) 0)))))))

(declaim (inline double-cell-unsigned))
(defun double-cell-unsigned (low-cell high-cell)
  (dpb high-cell (byte 64 64) (ldb (byte 64 0) low-cell)))
