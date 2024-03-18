(in-package #:forth)

;;; File-Access Words as defined in Section 11 of the Forth 2012 specification

(define-word binary-access-method (:word "BIN")
  "( fam1 - fam2 )"
  "Modify the implementation-defined file access method FAM1 to additionally select a “binary”, i.e., not line oriented,"
  "file access method, giving access method FAM2"
  (stack-push data-stack (logior +binary-mode+ (stack-pop data-stack))))

(define-word close-file (:word "CLOSE-FILE")
  "( fileid - ior )"
  "Close the file identified by FILEID. IOR is the implementation-defined I/O result code"
  (stack-push data-stack (forth-close-file files (stack-pop data-stack))))

(define-word create-file (:word "CREATE-FILE")
  "( c-addr u fam - fileid ior )"
  "Create the file named in the character string specified by C-ADDR and U, and open it with file access method FAM."
  "The meaning of values of FAM is implementation defined. If a file with the same name already exists, recreate it"
  "as an empty file.  the file was successfully created and opened, IOR is zero, FILEID is its identifier, and the file"
  "has been positioned to the start of the file."
  "Otherwise, IOR is the implementation-defined I/O result code and FILEID is undefined"
  (let ((fam (stack-pop data-stack))
        (length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of filename/pathname must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (multiple-value-bind (fileid ior)
          (forth-create-file files (forth-string-to-native forth-memory offset length) fam)
        (stack-push data-stack fileid)
        (stack-push data-stack ior)))))

;;;---*** DELETE-FILE

(define-word file-position (:word "FILE-POSITION")
  "( fileid – ud ior )"
  "UD is the current file position for the file identified by FILEID. IOR is the implementation-defined I/O result code."
  "UD is undefined if IOR is non-zero"
  (let ((fileid (stack-pop data-stack)))
    (multiple-value-bind (position ior)
        (forth-file-position files fileid)
      (stack-push-double data-stack position)
      (stack-push data-stack ior))))

(define-word file-size (:word "FILE-SIZE")
  "( fileid – ud ior )"
  "UD is the size, in characters, of the file identified by FILEID. IOR is the implementation-defined I/O result code"
  (let ((fileid (stack-pop data-stack)))
    (multiple-value-bind (size ior)
        (forth-file-size files fileid)
      (stack-push-double data-stack size)
      (stack-push data-stack ior))))

(define-word include-file (:word "INCLUDE-FILE")
  "( i*x fileid - j*x )"
  "Remove FILEID from the stack. Save the current input source specification, including the current value of SOURCE-ID. Store"
  "FILEID in SOURCE-ID. Make the file specified by FILEID the input source. Other stack effects are due to the words included."
  "Repeat until end of file: read a line from the file, fill the input buffer from the contents of that line, set >IN to zero,"
  "and interpret. Text interpretation begins at the file position where the next file read would occur."
  "When the end of the file is reached, close the file and restore the input source specification to its saved value"
  (source-push files :fileid (stack-pop data-stack)))

(define-word included (:word "INCLUDED")
  "( i*x c-addr u - j*x )"
  "Remove C-ADDR U from the stack. Save the current input source specification, including the current value of SOURCE-ID."
  "Open the file specified by C-ADDR U, store the resulting fileid in SOURCE-ID, and make it the input source. Other stack"
  "effects are due to the words included."
  "Repeat until end of file: read a line from the file, fill the input buffer from the contents of that line, set >IN to zero,"
  "and interpret. Text interpretation begins at the start of the file."
  "When the end of the file is reached, close the file and restore the input source specification to its saved value"
  (let ((length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of filename/pathname must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (let ((filename (forth-string-to-native forth-memory offset length)))
        ;; NOTE: Unlike INCLUDE, do not add the ".4th" file type to the filename
        ;;(unless (let ((pos (search ".4th" filename :from-end t)))
        ;;          (and pos (= pos (- (length filename) (length ".4th")))))
        ;;  (setf filename (concatenate 'string filename ".4th")))
        (multiple-value-bind (fileid ior)
            (forth-open-file files filename +read-direction+)
          (cond ((zerop ior)
                 (source-push files :fileid fileid))
                ((probe-file filename)
                 (forth-exception :file-i/o-exception "Can't open ~A" filename))
                (t
                 (forth-exception :file-not-found "~A not found" filename))))))))

(define-word open-file (:word "OPEN-FILE")
  "( c-addr u fam - fileid ior )"
  "Open the file named in the character string specified by C-ADDR U, with file access method indicated by FAM."
  "The meaning of values of fam is implementation defined. If the file is successfully opened, IOR is zero, FILEID is"
  "its identifier, and the file has been positioned to the start of the file."
  "Otherwise, IOR is the implementation-defined I/O result code and FILEID is undefined"
  (let ((fam (stack-pop data-stack))
        (length (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (unless (plusp length)
      (forth-exception :invalid-numeric-argument "Length of filename/pathname must be positive"))
    (multiple-value-bind (forth-memory offset)
        (memory-decode-address memory address)
      (multiple-value-bind (fileid ior)
          (forth-open-file files (forth-string-to-native forth-memory offset length) fam)
        (stack-push data-stack fileid)
        (stack-push data-stack ior)))))

(define-word read-only-file-access-method (:word "R/O")
  "( - fam )"
  "FAM is the implementation-defined value for selecting the 'read only' file access method"
  (stack-push data-stack +read-direction+))

(define-word read-write-file-access-method (:word "R/W")
  "( - fam )"
  "FAM is the implementation-defined value for selecting the 'read/write' file access method"
  (stack-push data-stack (logior +read-direction+ +write-direction+)))

(define-word read-file (:word "READ-FILE")
  "( c-addr u1 fileid – u2 ior )"
  "Read U1 consecutive characters to C-ADDR from the current position of the file identified by FILEID."
  "If U1 characters are read without an exception, IOR is zero and U2 is equal to U1."
  "If the end of the file is reached before U1 characters are read, IOR is zero and U2 is the"
  "number of characters actually read."
  "If the operation is initiated when the value returned by FILE-POSITION is equal to the"
  "value returned by FILE-SIZE for the file identified by fileid, IOR is zero and U2 is zero."
  "If an exception occurs, IOR is the implementation-defined I/O result code, and U2 is the"
  "number of characters transferred to C-ADDR without an exception"
  (let ((fileid (stack-pop data-stack))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to READ-FILE can't be negative"))
    (multiple-value-bind (region offset)
        (memory-decode-address memory address)
      (multiple-value-bind (read-count ior)
          (forth-read-file files fileid region offset count)
        (stack-push data-stack read-count)
        (stack-push data-stack ior)))))

(define-word read-line (:word "READ-LINE")
  "( c-addr u1 fileid – u2 flag ior )"
  "Read the next line from the file specified by FILEID into memory at the address C-ADDR. At most U1 characters are read."
  "Up to two implementation-defined line-terminating charac ters may be read into memory at the end of the line, but are not"
  "included in the count U2. The line buffer provided by C-ADDR should be at least U1+2 characters long."
  "If the operation succeeded, FLAG is true and IOR is zero. If a line terminator was received before U1 characters were read,"
  "then U2 is the number of characters, not including the line terminator, actually read (0 <= U2 <= U1)."
  "When U1 = U2 the line terminator has yet to be reached."
  "If the operation is initiated when the value returned by FILE-POSITION is equal to the value returned by FILE-SIZE"
  "for the file identified by FILEID, FLAG is false, IOR is zero, and U2 is zero. If IOR is non-zero, an exception occurred"
  "during the operation and IOR is the implementation-defined I/O result code"
  (let ((fileid (stack-pop data-stack))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to READ-LINE can't be negative"))
    (multiple-value-bind (line eof? ior)
        (forth-read-line files fileid count)
      (cond ((= ior +file-operation-success+)
             (cond (eof?
                    (stack-push data-stack 0)
                    (stack-push data-stack +false+))
                   (t
                    ;; Read was successful -- FORTH-READ-LINE will never return more the U1 characters
                    (multiple-value-bind (region offset)
                        (memory-decode-address memory address)
                      (native-into-forth-string line region offset))
                    (stack-push data-stack (length line))
                    (stack-push data-stack +true+))))
            (t
             (stack-push data-stack 0)
             (stack-push data-stack +false+)))
      (stack-push data-stack ior))))

(define-word reposition-file (:word "REPOSITION-FILE")
  "( ud fileid – ior )"
  "Reposition the file identified by FILEID to UD. IOR is the implementation-defined I/O result code"
  (let ((fileid (stack-pop data-stack))
        (position (stack-pop-double data-stack)))
    (when (minusp position)
      (forth-exception :invalid-numeric-argument "REPOSITION-FILE position can't be negative"))
    (stack-push data-stack (forth-file-reposition files fileid position))))

;;;---*** RESIZE-FILE

(define-word source-id (:word "SOURCE-ID")
  "( - 0 | -1 | fileid )"
  "Return 0 if the input source is the console, -1 if it is a string (EVALUATE), or the FILEID if it is a file"
  (stack-push data-stack (source-id files)))

(define-word write-only-file-access-method (:word "W/O")
  "( - fam )"
  "FAM is the implementation-defined value for selecting the 'write only' file access method"
  (stack-push data-stack +write-direction+))

(define-word write-file (:word "WRITE-FILE")
  "( c-addr u fileid – ior )"
  "Write U characters from C-ADDR to the file identified by FILEID starting at its current position."
  "IOR is the implementation-defined I/O result code"
  (let ((fileid (stack-pop data-stack))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to READ-FILE can't be negative"))
    (multiple-value-bind (region offset)
        (memory-decode-address memory address)
      (stack-push data-stack (forth-write-file files fileid region offset count)))))

(define-word write-line (:word "WRITE-LINE")
  "( c-addr u fileid – ior )"
  "Write U characters from C-ADDR followed by the implementation-dependent line terminator to the file identified by FILEID"
  "starting at its current position. IOR is the implementation- defined I/O result code"
  (let ((fileid (stack-pop data-stack))
        (count (cell-signed (stack-pop data-stack)))
        (address (stack-pop data-stack)))
    (when (minusp count)
      (forth-exception :invalid-numeric-argument "Count to WRITE-LINE can't be negative"))
    (multiple-value-bind (region offset)
        (memory-decode-address memory address)
      (stack-push data-stack (forth-write-line files fileid (forth-string-to-native region offset count))))))


;;; File-Access extension words as defined in Section 11 of the Forth 2012 specification

;;;---*** FILE-STATUS
;;;---*** FLUSH-FILE

(define-word include (:word "INCLUDE")
  "INCLUDE <filename>"
  "Skip leading white space and parse NAME delimited by a white space character. Push the address and length"
  "of the NAME on the stack and perform the function of INCLUDED"
  (let ((filename (word files #\Space)))
    (when (null filename)
      (forth-exception :zero-length-name "Filename must be supplied"))
    (unless (let ((pos (search ".4th" filename :from-end t)))
              (and pos (= pos (- (length filename) (length ".4th")))))
      (setf filename (concatenate 'string filename ".4th")))
    (multiple-value-bind (fileid ior)
        (forth-open-file files filename +read-direction+)
      (cond ((zerop ior)
             (source-push files :fileid fileid))
            ((probe-file filename)
             (forth-exception :file-i/o-exception "Can't open ~A" filename))
            (t
             (forth-exception :file-not-found "~A not found" filename))))))
    
(define-word refill (:word "REFILL")
  "Attempt to fill the input buffer from the input source, returning a TRUE flag if successful."
  "When the input source is the user input device, attempt to receive input into the terminal input buffer. If successful,"
  "make the result the input buffer, set >IN to zero, and return TRUE. Receipt of a line containing no characters is"
  "considered successful. If there is no input available from the current input source, return FALSE."
  "When the input source is a string from EVALUATE, return FALSE and perform no other action."
  "When the input source is a text file, attempt to read the next line from the text-input file. If successful, make"
  "the result the current input buffer, set >IN to zero, and return TRUE. Otherwise return FALSE"
  (stack-push data-stack (if (refill files) +true+ +false+)))

;;;---*** RENAME-FILE
;;;---*** REQUIRE
;;;---*** REQUIRED
