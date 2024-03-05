(in-package #:forth)

;;; Paragraph numbers refer to the Forth Programmer's Handbook, 3rd Edition

;;; 5.5.2 Global File Operaations

(define-word close-file (:word "CLOSE-FILE")
  "( file-id - ior )"
  "Close the file identified by FILE-ID and return an I/O result code"
  (stack-push data-stack (forth-close-file files (stack-pop data-stack))))

(define-word create-file (:word "CREATE-FILE")
  "( c-addr u fam - file-id ior )"
  "Create the file identified by the filename/pathname at C-ADDR of length U using the file access method FAM."
  "If the file already exists, re-create it as an empty file that replaces the existing file."
  "If successful, return the FILE-ID and IOR of zero. If opening fails, return a non-zero IOR and undefined FILE-ID"
  (let ((fam (stack-pop data-stack))
        (length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of filename/pathname must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (multiple-value-bind (file-id ior)
          (create-file files (forth-string-to-native forth-memory offset length) fam)
        (stack-push data-stack file-id)
        (stack-push data-stack ior)))))

;;;---*** DELETE-FILE
;;;---*** FLUSH-FILE

(define-word open-file (:word "OPEN-FILE")
  "( c-addr u fam - file-id ior )"
  "Open the file identified by the filename/pathname at C-ADDR of length U using the file access method FAM."
  "If successful, return the FILE-ID and IOR of zero. If opening fails, return a non-zero IOR and undefined FILE-ID"
  (let ((fam (stack-pop data-stack))
        (length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of filename/pathname must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (multiple-value-bind (file-id ior)
          (forth-open-file files (forth-string-to-native forth-memory offset length) fam)
        (stack-push data-stack file-id)
        (stack-push data-stack ior)))))

;;;---*** RENAME-FILE
;;;---*** RESIZE-FILE


;;; 5.5.3 File Reading and Writing

(define-word include-file (:word "INCLUDE-FILE")
  "( file-id )"
  "Save the current input source specification. Loop reading lines from the file identified by FILE-ID, starting at its"
  "current position, and interpreting them. On reaching the end of the file, restore the previous input source."
  (source-push files :file-id (stack-pop data-stack)))

(define-word included (:word "INCLUDED")
  "( c-addr u - )"
  "Similar to INCLUDE-FILE but, instead of a FILE-ID, open using the R/O file access method and interpret the file"
  "whose filename/pathname is at C-ADDR and of length U."
  (let ((length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of filename/pathname must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (let ((filename (forth-string-to-native forth-memory offset length)))
        (unless (let ((pos (search ".4th" filename :from-end t)))
                  (and pos (= pos (- (length filename) (length ".4th")))))
          (setf filename (concatenate 'string filename ".4th")))
        (multiple-value-bind (file-id ior)
            (forth-open-file files filename +read-direction+)
          (cond ((zerop ior)
                 (source-push files :file-id file-id))
                ((probe-file filename)
                 (forth-exception :file-i/o-exception "Can't open ~A" filename))
                (t
                 (forth-exception :file-not-found "~A not found" filename))))))))

(define-word include (:word "INCLUDE")
  "INCLUDE <filename>"
  "Simliar to INCLUDED but the filename/pathname is parsed from the input stream"
  (let ((filename (word files #\Space)))
    (when (null filename)
      (forth-exception :zero-length-name "Filename must be supplied"))
    (unless (let ((pos (search ".4th" filename :from-end t)))
              (and pos (= pos (- (length filename) (length ".4th")))))
      (setf filename (concatenate 'string filename ".4th")))
    (multiple-value-bind (file-id ior)
        (forth-open-file files filename +read-direction+)
      (cond ((zerop ior)
             (source-push files :file-id file-id))
            ((probe-file filename)
             (forth-exception :file-i/o-exception "Can't open ~A" filename))
            (t
             (forth-exception :file-not-found "~A not found" filename))))))
    
;;;---*** READ-FILE
;;;---*** READ-LINE
;;;---*** REFILL
;;;---*** WRITE-FILE
;;;---*** WRITE-LINE


;;; 5.5.4 File Support Words

(define-word binary-access-method (:word "BIN")
  "( fam1 - fam2 )"
  "Modify the file access method FAM1 to additionally select a binary access method and return the new access method FAM2"
  (stack-push data-stack (logior +binary-mode+ (stack-pop data-stack))))

;;;---*** FILE-POSITION
;;;---*** FILE-SIZE
;;;---*** FILE-STATUS

(define-word read-only-file-access-method (:word "R/O")
  "( - fam )"
  "Return the read-only file access method"
  (stack-push data-stack +read-direction+))

(define-word read-write-file-access-method (:word "R/W")
  "( - fam )"
  "Return the read/write file access method"
  (stack-push data-stack (logior +read-direction+ +write-direction+)))

;;;---*** REPOSITION-FILE

(define-word write-only-file-access-method (:word "W/O")
  "( - fam )"
  "Return the write-only file access method"
  (stack-push data-stack +write-direction+))
