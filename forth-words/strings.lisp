(in-package #:forth)

;;; String Word Set as defined in Section 17 of the Forth 2012 specification

(define-word trailing-spaces (:word "-TRAILING")
  "( c-addr u1 – c-addr u2 )"
  "If U1 is greater than zero, U2 is equal to U1 less the number of spaces at the end of the character string"
  "specified by C-ADDR U1. If U1 is zero or the entire string consists of spaces, U2 is zero"
  (let* ((count (cell-signed (stack-pop data-stack)))
         (address (stack-cell data-stack 0)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to -TRAILING can't be negative"))
          ((zerop count)
           (stack-push data-stack 0))
          (t
           ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
           (multiple-value-bind (region offset)
               (memory-decode-address memory address)
             (let* ((end (+ offset count))
                    (last (position +forth-char-space+ region :start offset :end end :from-end t :test-not #'=)))
               (stack-push data-stack (if last
                                          (- count (- end last 1))
                                          0))))))))

(define-word string-increment (:word "/STRING")
  "( c-addr1 u1 n – c-addr2 u2 )"
  "Adjust the character string at C-ADDR1 by N characters. The resulting character string, specified by C-ADDR2 U2, begins"
  "at C-ADDR1 plus N characters and is U1 minus N characters long"
  (let ((n (cell-signed (stack-pop data-stack)))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to /STRING can't be negative"))
          ((zerop count)
           (stack-push data-stack address)
           (stack-push data-stack count))
          (t
           (stack-push data-stack (+ address (* n +char-size+)))
           (stack-push data-stack (- count (* n +char-size+)))))))

(define-word write-blanks (:word "BLANK")
  "( c-addr u - )"
  "If U is greater than zero, store the character value for space in U consecutive character positions beginning at C-ADDR"
  (let ((count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to BLANK can't be negative"))
          ((zerop count))
          (t
           ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
           (memory-fill memory address count +forth-char-space+)))))

(define-word cmove (:word "CMOVE")
  "( c-addr1 c-addr2 u – )"
  "If U is greater than zero, copy U consecutive characters from the data space starting at C-ADDR1 to that starting"
  "at C-ADDR2, proceeding character-by-character from lower addresses to higher addresses"
  (let ((count (cell-signed (stack-pop data-stack)))
        (address2 (stack-pop data-stack))
        (address1 (stack-pop data-stack)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to CMOVE can't be negative"))
          ((zerop count))
          (t
           ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
           (dotimes (i count)
             (setf (memory-byte memory (+ address2 i)) (memory-byte memory (+ address1 i))))))))


(define-word cmove> (:word "CMOVE>")
  "( c-addr1 c-addr2 u – )"
  "If U is greater than zero, copy U consecutive characters from the data space starting at C-ADDR1 to that starting"
  "at C-ADDR2, proceeding character-by-character from higher addresses to lower addresses"
  (let ((count (cell-signed (stack-pop data-stack)))
        (address2 (stack-pop data-stack))
        (address1 (stack-pop data-stack)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to CMOVE> can't be negative"))
          ((zerop count))
          (t
           ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
           (dotimes (i count)
             (setf (memory-byte memory (+ address2 (- count i 1))) (memory-byte memory (+ address1 (- count i 1)))))))))

(define-word compare (:word "COMPARE")
  "( c-addr1 u1 c-addr2 u2 – n )"
  "Compare the string specified by C-ADDR1 U1 to the string specified by C-ADDR2 U2. The strings are compared, beginning"
  "at the given addresses, character by character, up to the length of the shorter string or until a difference is found."
  "If the two strings are identical, N is zero. If the two strings are identical up to the length of the shorter string,"
  "N is minus- one (-1) if U1 is less than U2 and one (1) otherwise. If the two strings are not identical up to the length"
  "of the shorter string, N is minus-one (-1) if the first non-matching character in the string specified BY C-ADDR1 U1"
  "has a lesser numeric value than the corresponding character in the string specified by C-ADDR2 U2 and one (1) otherwise"
  (let ((count2 (cell-signed (stack-pop data-stack)))
        (address2 (stack-pop data-stack))
        (count1 (cell-signed (stack-pop data-stack)))
        (address1 (stack-pop data-stack)))
    (when (or (minusp count1) (minusp count2))
      (forth-exception :invalid-numeric-argument "Count to SEARCH can't be negative"))
    (multiple-value-bind (region1 offset1)
        (memory-decode-address memory address1)
      (multiple-value-bind (region2 offset2)
          (memory-decode-address memory address2)
        ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
        (let* ((end1 (+ offset1 count1))
               (end2 (+ offset2 count2))
               (position (mismatch region1 region2 :start1 offset1 :end1 end1 :start2 offset2 :end2 end2))
               (short-end1 (+ offset1 (min count1 count2)))
               (short-end2 (+ offset2 (min count1 count2)))
               (short-position (mismatch region1 region2 :start1 offset1 :end1 short-end1 :start2 offset2 :end2 short-end2)))
          (cond ((null position)
                 ;; Strings are of equal length and identical
                 (stack-push data-stack 0))
                ((null short-position)
                 ;; Strings are identical up to length of shorter string
                 (if (< count1 count2)
                     (stack-push data-stack -1)
                     (stack-push data-stack 1)))
                (t
                 ;; Strings are not identical up to length of shorter string
                 ;; NOTE: POSITION is position in the first string so to convert it to to an offset, subtract OFFSET1
                 (let ((c1 (memory-byte memory (+ address1 (- position offset1))))
                       (c2 (memory-byte memory (+ address2 (- position offset1)))))
                   (if (< c1 c2)
                       (stack-push data-stack -1)
                       (stack-push data-stack 1))))))))))

(define-word search (:word "SEARCH")
  "( c-addr1 u1 c-addr2 u2 – c-addr3 u3 flag )"
  "Search the string specified by C-ADDR1 U1 for the string specified by C-ADDR2 U2."
  "If FLAG is true, a match was found at C-ADDR3 with U3 characters remaining."
  "If FLAG is false there was no match and C-ADDR3 is C-ADDR1 and U3 is U1"
  (let ((count2 (cell-signed (stack-pop data-stack)))
        (address2 (stack-pop data-stack))
        (count1 (cell-signed (stack-pop data-stack)))
        (address1 (stack-pop data-stack)))
    (when (or (minusp count1) (minusp count2))
      (forth-exception :invalid-numeric-argument "Count to SEARCH can't be negative"))
    (multiple-value-bind (region1 offset1)
        (memory-decode-address memory address1)
      (multiple-value-bind (region2 offset2)
          (memory-decode-address memory address2)
        ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
        (let* ((end1 (+ offset1 count1))
               (end2 (+ offset2 count2))
               (position (search region2 region1 :start1 offset2 :end1 end2 :start2 offset1 :end2 end1)))
          (cond ((null position)
                 (stack-push data-stack address1)
                 (stack-push data-stack count1)
                 (stack-push data-stack +false+))
                (t
                 (let ((offset (- position offset1)))
                   (stack-push data-stack (+ address1 offset))
                   (stack-push data-stack (- count1 offset))
                   (stack-push data-stack +true+)))))))))

(define-word sliteral (:word "SLITERAL" :immediate? t :compile-only? t)
  "Compilation: ( c-addr1 u – )" "Run-time: ( – c-addr2 u )"
  "Return C-ADDR2 U describing a string consisting of the characters specified by C-ADDR1 U during compilation"
  (let* ((count (cell-signed (stack-pop data-stack)))
         (address (stack-pop data-stack)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to SLITERAL can't be negative"))
          (t
           (let ((runtime-address (allocate-memory memory (* count +char-size+))))
             (memory-copy memory address runtime-address (* count +char-size+))
             (add-to-definition fs
               `(stack-push data-stack ,runtime-address)
               `(stack-push data-stack ,count)))))))

(define-word replaces (:word "REPLACES")
  "( c-addr1 u1 c-addr2 u2 – )"
  "Set the string C-ADDR1 U1 as the text to substitute for the substitution named by C-ADDR2 U2."
  "If the substitution does not exist it is created"
  (let ((count2 (cell-signed (stack-pop data-stack)))
        (address2 (stack-pop data-stack))
        (count1 (cell-signed (stack-pop data-stack)))
        (address1 (stack-pop data-stack)))
    (when (minusp count1)
      (forth-exception :invalid-numeric-argument "Substitution's length in REPLACES can't be negative"))
    (unless (plusp count2)
      (forth-exception :invalid-numeric-argument "Original's length in REPLACES must be positive"))
    (multiple-value-bind (region1 offset1)
        (memory-decode-address memory address1)
      (multiple-value-bind (region2 offset2)
          (memory-decode-address memory address2)
        (let ((substitution (forth-string-to-native region1 offset1 count1))
              (name (forth-string-to-native region2 offset2 count2)))
          (register-replacement replacements name substitution))))))

(define-word substitute (:word "SUBSTITUTE")
  "( c-addr1 u1 c-addr2 u2 – c-addr2 u3 n )"
  "Perform substitution on the string C-ADDR1 U1 placing the result at string C-ADDR2 U3, where U3 is the length"
  "of the resulting string. An error occurs if the resulting string will not fit into C-ADDR2 U2 or if C-ADDR2 is"
  "the same as C-ADDR1. The return value N is positive or 0 on success and indicates the number of substitutions made."
  "A negative value for N indicates that an error occurred, leaving C-ADDR2 U3 undefined"
  (let ((count2 (cell-signed (stack-pop data-stack)))
        (address2 (stack-pop data-stack))
        (count1 (cell-signed (stack-pop data-stack)))
        (address1 (stack-pop data-stack)))
    (when (or (minusp count1) (minusp count2))
      (forth-exception :invalid-numeric-argument "Count in SUBSTITUTE can't be negative"))
    ;; Don't have to check for overlapping input and output as we perform the substitution in
    ;; a temporary area and then move the result into the output buffer
    (multiple-value-bind (region1 offset1)
        (memory-decode-address memory address1)
      (multiple-value-bind (region2 offset2)
          (memory-decode-address memory address2)
        (let ((input (forth-string-to-native region1 offset1 count1)))
          (multiple-value-bind (output n-replacements)
              (perform-substitute replacements input)
            (let ((output-length (length output)))
              (cond ((minusp n-replacements)
                     ;; Negative replacement count indicates and error
                     (stack-push data-stack 0)
                     (stack-push data-stack 0)
                     (stack-push data-stack n-replacements))
                    ((> output-length count2)
                     (stack-push data-stack 0)
                     (stack-push data-stack 0)
                     (stack-push data-stack (forth-exception-key-to-code :substitute-exception)))
                    (t
                     (native-into-forth-string output region2 offset2)
                     (stack-push data-stack address2)
                     (stack-push data-stack output-length)
                     (stack-push data-stack n-replacements))))))))))
                   
(define-word unescape (:word "UNESCAPE")
  "( c-addr1 u1 c-addr2 – c-addr2 u2 )"
  "Replace each ‘%’ character in the input string C-ADDR1 U1 by two ‘%’ characters. The output is represented by C-ADDR2 U2"
  (let* ((address2 (stack-pop data-stack))
         (count (cell-signed (stack-pop data-stack)))
         (address1 (stack-pop data-stack)))
    (cond ((minusp count)
           (forth-exception :invalid-numeric-argument "Count to UNESCAPE can't be negative"))
          (t
           (let ((i2 0))
             ;; NOTE: Relies on the fact that +CHAR-SIZE+ is 1
             (dotimes (i1 count)
               (let ((ch (memory-byte memory (+ address1 i1))))
                 (setf (memory-byte memory (+ address2 i2)) ch)
                 (incf i2)
                 (when (= ch +forth-char-escape+)
                   (setf (memory-byte memory (+ address2 i2)) +forth-char-escape+)
                   (incf i2))))
             (stack-push data-stack address2)
             (stack-push data-stack i2))))))
