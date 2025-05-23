;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://opensource.org/license/mit

(in-package #:forth)

(defconstant +true+ -1)
(defconstant +false+ 0)

(declaim (inline truep))
(defun truep (x) (not (zerop x)))

(declaim (inline falsep))
(defun falsep (x) (zerop x))

(defconstant +most-positive-single-cell+ (1- (dpb 1 (byte 1 63) 0)))
(defconstant +most-negative-single-cell+ (- (dpb 1 (byte 1 63) 0)))
(defconstant +maximum-unsigned-single-cell+ (1- (dpb 1 (byte 1 64) 0)))

(defconstant +most-positive-double-cell+ (1- (dpb 1 (byte 1 127) 0)))
(defconstant +most-negative-double-cell+ (- (dpb 1 (byte 1 127) 0)))
(defconstant +maximum-unsigned-double-cell+ (1- (dpb 1 (byte 1 128) 0)))

(defun interpret-number (token base &key (allow-floats? t) (signal-overflow? t))
  (flet ((interpret-base-prefix ()
           (let ((ch (aref token 0)))
             (cond ((eql ch #\#) (values 10 1))
                   ((eql ch #\$) (values 16 1))
                   ((eql ch #\%) (values  2 1))
                   (t            (values base 0))))))
    (cond ((and allow-floats?
                (= base 10)
                (let ((ch (aref token 0)))
                  (or (digit-char-p ch)
                      (eql ch #\+)
                      (eql ch #\-)))
                (= (count #\E token :test #'char-equal) 1))
           ;; When BASE is 10 and the string contains exactly one "E", try floating point
           (let* ((token (if (char-equal (aref token (1- (length token))) #\E)
                             (concatenate 'string token "0")
                             token))
                  (float (with-standard-io-syntax
                           (let ((*read-default-float-format* 'double-float)
                                 (*read-eval* nil))
                             (read-from-string token)))))
             (if (floatp float)
                 (values :float float)
                 (values nil nil))))
          ;; 'c' is interpreted as a character literal
          ((and (= (length token) 3)
                (eql (aref token 0) #\')
                (eql (aref token 2) #\'))
           (values :single (forth-char (aref token 1))))
          ((and (= (count #\. token :test #'eql) 1)
                (eql (aref token (1- (length token))) #\.))
           ;; If the string end with a period, try a double precision integer
           (handler-case
               (multiple-value-bind (base start)
                   (interpret-base-prefix)
                 (let ((value (parse-integer token :radix base :start start :end (1- (length token)))))
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
                 (let ((value (parse-integer token :radix base :start start)))
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

(define-compiler-macro cell-signed (&whole form &environment env cell)
  (if (constantp cell env)
      (cell-signed (eval cell))
      form))

(declaim (inline cell-unsigned))
(defun cell-unsigned (cell)
  (ldb (byte 64 0) cell))

(define-compiler-macro cell-unsigned (&whole form &environment env cell)
  (if (constantp cell env)
      (cell-unsigned (eval cell))
      form))

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

(define-compiler-macro double-cell-signed (&whole form &environment env low-cell high-cell)
  (if (and (constantp low-cell env) (constantp high-cell env))
      (double-cell-signed (eval low-cell) (eval high-cell))
      form))

(declaim (inline double-cell-unsigned))
(defun double-cell-unsigned (low-cell high-cell)
  (dpb high-cell (byte 64 64) (ldb (byte 64 0) low-cell)))

(define-compiler-macro double-cell-unsigned (&whole form &environment env low-cell high-cell)
  (if (and (constantp low-cell env) (constantp high-cell env))
      (double-cell-unsigned (eval low-cell) (eval high-cell))
      form))

(declaim (inline quad-byte-signed))
(defun quad-byte-signed (value)
  (let ((raw (ldb (byte 32 0) value)))
    (if (zerop (ldb (byte 1 31) raw))
        raw
        (- raw (dpb 1 (byte 1 32) 0)))))

(define-compiler-macro quad-byte-signed (&whole form &environment env value)
  (if (constantp value env)
      (quad-byte-signed (eval value))
      form))

(declaim (inline quad-byte-unsigned))
(defun quad-byte-unsigned (value)
  (ldb (byte 32 0) value))

(define-compiler-macro quad-byte-unsigned (&whole form &environment env value)
  (if (constantp value env)
      (quad-byte-unsigned (eval value))
      form))

(declaim (inline double-byte-signed))
(defun double-byte-signed (value)
  (let ((raw (ldb (byte 16 0) value)))
    (if (zerop (ldb (byte 1 15) raw))
        raw
        (- raw (dpb 1 (byte 1 16) 0)))))

(define-compiler-macro double-byte-signed (&whole form &environment env value)
  (if (constantp value env)
      (double-byte-signed (eval value))
      form))

(declaim (inline double-byte-unsigned))
(defun double-byte-unsigned (value)
  (ldb (byte 16 0) value))

(define-compiler-macro double-byte-unsigned (&whole form &environment env value)
  (if (constantp value env)
      (double-byte-unsigned (eval value))
      form))


;;; Floating Point

(declaim (inline >single-float))
(defun >single-float (x)
  (float x 1.0d0))

(defun decode-single-float (f)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float f)
    (let ((exponent (+ exponent 127 23)))
      (when (and (= exponent 1) (zerop (ldb (byte 1 23) significand)))
        ;; Zero or a subnormal number
        (setf exponent 0))
      (dpb (if (minusp sign) 1 0) (byte 1 31) (dpb exponent (byte 8 23) (ldb (byte 23 0) significand))))))

(defun encode-single-float (n)
  (let ((sign (ldb (byte 1 31) n))
        (exponent (ldb (byte 8 23) n))
        (significand (ldb (byte 23 0) n)))
    (cond ((= exponent 255)
           ;; Encoding of either a NaN or an infinity
           (forth-exception :floating-out-of-range))
          ((zerop exponent)
           ;; Encoding of zero or a subnormal number
           (setf exponent 1))
          (t
           (setf significand (dpb 1 (byte 1 23) significand))))
    (let ((absolute (scale-float (float significand 1.0e0) (- exponent 127 23))))
      (if (zerop sign)
          absolute
          (- absolute)))))

(declaim (inline >double-float))
(defun >double-float (x)
  (float x 1.0d0))

(defun decode-double-float (f)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float f)
    (let ((exponent (+ exponent 1023 52)))
      (when (and (= exponent 1) (zerop (ldb (byte 1 52) significand)))
        ;; Zero or a subnormal number
        (setf exponent 0))
      (dpb (if (minusp sign) 1 0) (byte 1 63) (dpb exponent (byte 11 52) (ldb (byte 52 0) significand))))))

(defun encode-double-float (n)
  (let ((sign (ldb (byte 1 63) n))
        (exponent (ldb (byte 11 52) n))
        (significand (ldb (byte 52 0) n)))
    (cond ((= exponent 2047)
           ;; Encoding of either a NaN or an infinity
           (forth-exception :floating-out-of-range))
          ((zerop exponent)
           ;; Encoding of zero or a subnormal number
           (setf exponent 1))
          (t
           (setf significand (dpb 1 (byte 1 52) significand))))
    (let ((absolute (scale-float (float significand 1.0d0) (- exponent 1023 52))))
      (if (zerop sign)
          absolute
          (- absolute)))))

;;; CL-Forth uses double precision floating point as its internal representation of float values

(defconstant +most-positive-native-float+ most-positive-double-float)

(declaim (inline native-float))
(defun native-float (x)
  (float x 1.0d0))

(declaim (inline decode-native-float))
(defun decode-native-float (f)
  (decode-double-float f))

(declaim (inline encode-native-float))
(defun encode-native-float (n)
  (encode-double-float n))

(defmacro with-native-float-format (() &body body)
  `(let ((*read-default-float-format* 'double-float))
     ,@body))
