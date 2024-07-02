;;; -*- Syntax: Common-Lisp; Base: 10 -*-
;;;
;;; Copyright (c) 2024 Gary Palter
;;;
;;; Licensed under the MIT License;
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;   https://github.com/gmpalter/cl-forth/tree/main?tab=MIT-1-ov-file#readme

(in-package #:forth)

(defparameter *forth-words-package* (find-package '#:forth-words))


;;; Dictionaries

(defclass dictionary ()
  ((name :accessor dictionary-name :initarg :name)
   (wid :accessor dictionary-wid :initarg :wid)
   (last-ordinal :initform -1)
   (words :accessor dictionary-words :initform (make-hash-table :test #'equalp))
   (parent :accessor dictionary-parent :initform nil :initarg :parent))
  )

(defmethod print-object ((dict dictionary) stream)
  (print-unreadable-object (dict stream :type t :identity t)
    (write-string (dictionary-name dict) stream)))

(defmethod add-word ((dict dictionary) word &key override silent)
  (with-slots (last-ordinal words parent) dict
    (let ((old (gethash (word-name word) words)))
      (when old
        (unless silent
          (format t "~A isn't unique. " (word-name word)))
        (unless override
          (setf (word-previous word) old)))
      ;; Don't change the word's ordinal if it already has one
      (if (= (word-ordinal word) -1)
          (setf (word-ordinal word) (incf last-ordinal))
          (setf last-ordinal (max last-ordinal (word-ordinal word))))
      (note-new-word parent dict word)
      (setf (gethash (word-name word) words) word))))

(defmethod delete-word ((dict dictionary) xts word)
  (with-slots (words) dict
    (let ((name (word-name word))
          (old (word-previous word)))
      (delete-execution-token xts word)
      (cond (old
             (if (eq (word-parent old) dict)
                 (setf (gethash name words) old)
                 (remhash name words))
             (reregister-execution-token xts (word-execution-token word)))
            (t
             (remhash name words))))))

(defmethod show-words ((dict dictionary))
  (let* ((words (dictionary-words dict))
         (names nil))
    (maphash #'(lambda (key word)
                 (declare (ignore word))
                 (push key names))
             words)
    (setf names (sort names #'string-lessp))
    (format t "~&In word list ~A (~D word~:P):~%  " (dictionary-name dict) (hash-table-count words))
    (loop with column = 2
          for name in names
          do (when (> (+ column (length name) 1) 120)
               (terpri)
               (write-string "  ")
               (setf column 2))
             (write-string name)
             (write-char #\Space)
             (incf column (1+ (length name)))
          finally
             (when (> column 2)
               (terpri)))))

(defmethod search-dictionary ((dict dictionary) name)
  (with-slots (words) dict
    (gethash name words)))

(defmethod forget-word ((dict dictionary) xts word)
  (with-slots (words) dict
    (let ((ordinal (word-ordinal word)))
      (maphash #'(lambda (key a-word)
                   (declare (ignore key))
                   (when (> (word-ordinal a-word) ordinal)
                     (delete-word dict xts a-word)))
               words))
    (delete-word dict xts word)))


;;; Word Lists and the Search Order

(defvar *predefined-words* (make-hash-table :test #'equalp))

(defclass word-lists ()
  ((all-word-lists :initform (make-hash-table :test #'equalp))
   (forth :reader word-lists-forth-word-list :initform nil)
   (search-order :accessor  word-lists-search-order :initform nil)
   (compilation-word-list :accessor word-lists-compilation-word-list :initform nil)
   (next-wid :initform (make-address #xFF 0))
   (wid-to-word-list-map :initform (make-hash-table))
   (next-nt :initform (make-address #xFE 0))
   (nt-to-word-map :initform (make-hash-table))
   (saved-word-lists :initform nil)
   (saved-search-order :initform nil)
   (saved-compilation-word-list :initform nil)
   (markers :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (context :initform 0)
   (current :initform 0))
  )

(defmethod print-object ((wls word-lists) stream)
  (with-slots (all-word-lists) wls
    (print-unreadable-object (wls stream :type t :identity t)
      (format stream "~D word list~:P" (hash-table-count all-word-lists)))))

(defmethod update-psuedo-state-variables ((wls word-lists))
  (with-slots (search-order compilation-word-list context current) wls
    (setf context (dictionary-wid (first search-order))
          current (dictionary-wid compilation-word-list))))

(defmethod install-predefined-words ((wls word-lists))
  (maphash #'(lambda (key wl-and-word)
               (declare (ignore key))
               (let ((wl (word-list wls (car wl-and-word) :if-not-found :create)))
                 (add-word wl (cdr wl-and-word) :override t :silent t)))
           *predefined-words*))

(defmethod register-predefined-words ((wls word-lists) execution-tokens here)
  (maphash #'(lambda (key wl-and-word)
               (declare (ignore key))
               (let ((word (cdr wl-and-word)))
                 (register-execution-token execution-tokens word here)
                 (loop for old-word = (word-previous word) then (word-previous old-word)
                       while old-word
                       do (register-execution-token execution-tokens old-word here))))
           *predefined-words*))

(defmethod reset-word-lists ((wls word-lists))
  (with-slots (all-word-lists forth search-order compilation-word-list wid-to-word-list-map nt-to-word-map
               saved-word-lists saved-search-order saved-compilation-word-list markers)
      wls
    (clrhash all-word-lists)
    (clrhash wid-to-word-list-map)
    (clrhash nt-to-word-map)
    (when saved-word-lists
      ;; This will create the FORTH word list, all word lists in the search order, the compilation word list,
      ;; and any other word lists that were created but currently not in use
      (dolist (name-and-wid saved-word-lists)
        (word-list wls (car name-and-wid) :if-not-found :create :saved-wid (cdr name-and-wid)))
      (setf search-order (loop for wl in saved-search-order
                               collect (word-list wls wl :if-not-found :create)))
      ;; In case the user just creates an empty word list and sets it as the compilation word list before GILDing
      (setf compilation-word-list (word-list wls saved-compilation-word-list :if-not-found :create))
      (update-psuedo-state-variables wls))
    (when (plusp (hash-table-count *predefined-words*))
      ;; Even if SAVED-WORD-LISTS is NIL, this next call will create the FORTH word list at a minimum
      (install-predefined-words wls)
      (when (null saved-word-lists)
        (setf forth (word-list wls "FORTH"))
        (setf search-order (list forth))
        (setf compilation-word-list forth)
        (update-psuedo-state-variables wls)))
    (setf markers (make-array 0 :fill-pointer 0 :adjustable t))))

(defmethod save-word-lists-state ((wls word-lists))
  (with-slots (all-word-lists search-order compilation-word-list
               saved-word-lists saved-search-order saved-compilation-word-list) wls
    (setf saved-word-lists nil)
    (maphash #'(lambda (name dictionary)
                 (push (cons name (dictionary-wid dictionary)) saved-word-lists))
             all-word-lists)
    (clrhash *predefined-words*)
    (maphash #'(lambda (dictionary-name dictionary)
                 (maphash #'(lambda (word-name word)
                              (let ((key (format nil "~A.~A" dictionary-name word-name)))
                                (setf (gethash key *predefined-words*) (cons dictionary-name word))))
                          (dictionary-words dictionary)))
             all-word-lists)
    (setf saved-search-order (map 'list #'dictionary-name search-order))
    (setf saved-compilation-word-list (dictionary-name compilation-word-list))))

(defmethod save-to-template ((wls word-lists))
  (with-slots (all-word-lists search-order compilation-word-list markers) wls
    (let ((template-word-lists nil)
          (template-words nil)
          (template-search-order nil)
          (template-compilation-word-list nil)
          (template-markers (make-array (fill-pointer markers))))
      (maphash #'(lambda (name dictionary)
                   (push (cons name (dictionary-wid dictionary)) template-word-lists))
               all-word-lists)
      (maphash #'(lambda (name dictionary)
                   (declare (ignore name))
                   (let ((words nil))
                     (maphash #'(lambda (word-name word)
                                  (declare (ignore word-name))
                                  (push word words)
                                  (loop for old-word = (word-previous word) then (word-previous old-word)
                                        while old-word
                                        do (push old-word words)))
                              (dictionary-words dictionary))
                     (setf words (sort words #'< :key #'word-ordinal))
                     (push (cons (dictionary-wid dictionary) words) template-words)))
               all-word-lists)
      (setf template-words (sort template-words #'< :key #'car))
      (setf template-search-order (map 'list #'dictionary-name search-order))
      (setf template-compilation-word-list (dictionary-name compilation-word-list))
      (dotimes (i (fill-pointer markers))
        (setf (aref template-markers i) (save-marker-to-template wls (aref markers i))))
      (list template-word-lists template-words template-search-order template-compilation-word-list template-markers))))

(defmethod load-from-template ((wls word-lists) template fs)
  (declare (ignore fs))
  (with-slots (all-word-lists forth search-order compilation-word-list wid-to-word-list-map nt-to-word-map markers) wls
    (clrhash all-word-lists)
    (clrhash wid-to-word-list-map)
    (clrhash nt-to-word-map)
    (clrhash *predefined-words*)
    (destructuring-bind (template-word-lists template-words template-search-order template-compilation-word-list
                         template-markers)
        template
      (dolist (name-and-wid template-word-lists)
        (word-list wls (car name-and-wid) :if-not-found :create :saved-wid (cdr name-and-wid)))
      (dolist (wid-and-words template-words)
        (let* ((wid (car wid-and-words))
               (wl (lookup-wid wls wid))
               (wl-name (dictionary-name wl))
               (words (cdr wid-and-words)))
          (dolist (word words)
            (let* ((word-name (word-name word))
                   (key (format nil "~A.~A" wl-name word-name)))
              (add-word wl word :silent t)
              (setf (gethash key *predefined-words*) (cons wl-name word))))))
      (setf forth (word-list wls "FORTH"))
      (setf search-order (loop for wl in template-search-order
                               collect (word-list wls wl)))
      (setf compilation-word-list (word-list wls template-compilation-word-list))
      (let ((n-markers (length template-markers)))
        (setf markers (make-array n-markers :fill-pointer n-markers :adjustable t))
        (dotimes (i n-markers)
          (setf (aref markers i) (load-marker-from-template wls (aref template-markers i)))))
      (update-psuedo-state-variables wls)))
  nil)

(defmethod word-list ((wls word-lists) name &key (if-not-found :error) (saved-wid nil))
  (with-slots (all-word-lists wid-to-word-list-map next-wid) wls
    (let ((name (or name (symbol-name (gensym "WL")))))
      (or (gethash name all-word-lists)
          (case if-not-found
            (:create
             (let* ((wid (or saved-wid next-wid))
                    (word-list (make-instance 'dictionary :name name :wid wid :parent wls)))
               (setf (gethash name all-word-lists) word-list
                     (gethash wid wid-to-word-list-map) word-list)
               (if saved-wid
                   (when (> saved-wid next-wid)
                     (setf next-wid (+ saved-wid +cell-size+)))
                   (incf next-wid +cell-size+))
               word-list))
            (:error
             (forth-exception :unknown-word-list "Word list ~A does not exist" name))
            (otherwise nil))))))

(defmethod lookup-wid ((wls word-lists) wid)
  (with-slots (wid-to-word-list-map) wls
    (or (gethash wid wid-to-word-list-map)
        (forth-exception :unknown-word-list "~14,'0X is not a wordlist id" wid))))

(defmethod lookup ((wls word-lists) token)
  (with-slots (search-order) wls
    (loop for dictionary in search-order
            thereis (loop for word = (gethash token (dictionary-words dictionary)) then (word-previous word)
                          while word
                            thereis (and (not (word-smudge? word)) word)))))

(defmethod lookup-nt ((wls word-lists) nt)
  (with-slots (nt-to-word-map) wls
    (or (gethash nt nt-to-word-map)
        (forth-exception :not-a-name-token "~14,'0X is not a name token" nt))))
        
(defmethod also ((wls word-lists))
  (with-slots (search-order) wls
    (push (first search-order) search-order)))

(defmethod definitions ((wls word-lists))
  (with-slots (search-order compilation-word-list) wls
    (setf compilation-word-list (first search-order))
    (update-psuedo-state-variables wls)))

(defmethod only ((wls word-lists))
  (with-slots (search-order forth) wls
    (setf search-order (list forth))
    (update-psuedo-state-variables wls)))

(defmethod previous ((wls word-lists))
  (with-slots (search-order) wls
    (when (< (length search-order) 2)
      (forth-exception :search-order-underflow))
    (pop search-order)
    (update-psuedo-state-variables wls)))

(defmethod vocabulary ((wls word-lists) name)
  (when (word-list wls name :if-not-found nil)
    (forth-exception :duplicate-word-list "~A is the name of an existing word list" name))
  ;; This will create the list as we already verified it doesn't exist
  (word-list wls name :if-not-found :create))

(defmethod replace-top-of-search-order ((wls word-lists) (dict dictionary))
  (with-slots (search-order) wls
    (setf (first search-order) dict)
    (update-psuedo-state-variables wls)))

(defmethod replace-top-of-search-order ((wls word-lists) (wid integer))
  (with-slots (wid-to-word-list-map) wls
    (let ((word-list (gethash wid wid-to-word-list-map)))
      (if word-list
          (replace-top-of-search-order wls word-list)
          (forth-exception :unknown-word-list "~14,'0X is not a wordlist id" wid)))))

(defmethod traverse-wordlist ((wls word-lists) wl function)
  (maphash #'(lambda (name word)
               (declare (ignore name))
               (if (funcall function (word-name-token word))
                   (loop for old = (word-previous word) then (word-previous old)
                         while old
                         do (when (eq (word-parent old) wl)
                              (unless (funcall function (word-name-token old))
                                (return-from traverse-wordlist))))
                   (return-from traverse-wordlist)))
           (dictionary-words wl)))


;;; Words

(defclass word ()
  ((name :accessor word-name :initarg :name)
   (previous :accessor word-previous :initarg :previous :initform nil)
   (smudge? :accessor word-smudge? :initarg :smudge? :initform nil)
   (immediate? :accessor word-immediate? :initarg :immediate? :initform nil)
   (compile-only? :accessor word-compile-only? :initarg :compile-only? :initform nil)
   (inlineable? :accessor word-inlineable? :initarg :inlineable? :initform nil)
   (created-word? :accessor word-created-word? :initarg :created-word? :initform nil)
   (deferring-word? :accessor word-deferring-word? :initarg :deferring-word? :initform nil)
   (code :accessor word-code :initarg :code :initform nil)
   (inline-forms :accessor word-inline-forms :initarg :inline-forms :initform nil)
   (parameters :accessor word-parameters :initarg :parameters :initform nil)
   (does> :accessor word-does> :initform nil)
   (parent :accessor word-parent :initform nil)
   (execution-token :accessor word-execution-token :initform nil)
   (compile-token :accessor word-compile-token :initform nil)
   (name-token :accessor word-name-token :initform nil)
   (ordinal :accessor word-ordinal :initform -1))
  )

(defmethod print-object ((word word) stream)
  (print-unreadable-object (word stream :type t :identity t)
    (write-string (or (word-name word) "<Anonymous>") stream)))

(defmacro define-word (name (&key (word-list "FORTH") ((:word forth-name) (symbol-name name))
                                  immediate? compile-only? (inlineable? (not compile-only?)))
                       &body body)
  (let* ((word (gensym))
         (key (format nil "~A.~A" word-list forth-name))
         (body (loop with forms = (copy-list body)
                     while (stringp (car forms))
                     do (pop forms)
                     finally (return forms)))
         (thunk `(named-lambda ,(intern forth-name *forth-words-package*) (fs &rest parameters)
                   (declare (ignorable parameters) (optimize (speed 3) (safety 0)))
                   (with-forth-system (fs)
                     ,@body))))
    `(eval-when (:load-toplevel :execute)
       (let ((,word (make-instance 'word
                                   :name ,forth-name
                                   :immediate? ,immediate?
                                   :compile-only? ,compile-only?
                                   :inlineable? ,inlineable?
                                   :code ,thunk
                                   :inline-forms ',(when inlineable?
                                                     (reverse body)))))
         (setf (gethash ,key *predefined-words*) (cons ,word-list ,word))))))

(defmacro define-state-word (slot &key (word-list "FORTH") ((:word forth-name) (symbol-name slot)) immediate? compile-only?)
  (let ((description (format nil "Place the address of ~A on the stack" forth-name)))
    `(define-word ,slot (:word-list ,word-list :word ,forth-name :immediate? ,immediate? :compile-only? ,compile-only?)
       "( - a-addr )"
       ,description
       (stack-push data-stack (state-slot-address memory ',slot)))))

(defun make-word (name code &key smudge? immediate? compile-only? created-word? deferring-word? parameters)
  (make-instance 'word :name name
                       :code code
                       :smudge? smudge?
                       :immediate? immediate?
                       :compile-only? compile-only?
                       :created-word? created-word?
                       :deferring-word? deferring-word?
                       :parameters (copy-list parameters)))

(defmethod forget ((word word) xts)
  (forget-word (word-parent word) xts word))


;;; MARKER support

(defclass marker ()
  ((search-order :reader marker-search-order :initarg :search-order :initform nil)
   (compilation-word-list :reader marker-compilation-word-list :initarg :compilation-word-list :initform nil)
   (words :initform nil)
   (included-files :initform nil))
  )

(defmethod add-word-to-marker ((marker marker) word)
  (with-slots (words) marker
    (push word words)))

(defmethod remove-words ((marker marker) xts)
  (with-slots (words) marker
    (map  nil #'(lambda (word) (delete-word (word-parent word) xts word)) words)
    (setf words nil)))

(defmethod add-included-file-to-marker ((marker marker) included-file)
  (with-slots (included-files) marker
    (push included-file included-files)))

(defmethod remove-included-files ((marker marker) files)
  (with-slots (included-files) marker
    (map nil #'(lambda (included-file) (forget-included-file files included-file)) included-files)
    (setf included-files nil)))

(defmethod register-marker ((wls word-lists))
  (with-slots (search-order compilation-word-list markers) wls
    (let ((marker (make-instance 'marker :search-order (copy-list search-order) :compilation-word-list compilation-word-list)))
      (vector-push-extend marker markers)
      marker)))

(defmethod execute-marker ((wls word-lists) xts files (marker marker))
  (with-slots (search-order compilation-word-list markers) wls
    (let ((position (position marker markers)))
      ;; Ignore stale markers
      (when position
        (setf search-order (copy-list (marker-search-order marker))
              compilation-word-list (marker-compilation-word-list marker))
        (remove-words marker xts)
        (remove-included-files marker files)
        (loop for i from position below (fill-pointer markers)
              do (setf (aref markers i) nil))
        (setf (fill-pointer markers) position)))))

(defmethod note-new-word ((wls word-lists) (dict dictionary) (word word))
  (with-slots (next-nt nt-to-word-map markers) wls
    (setf (word-parent word) dict)
    (cond ((word-name-token word)
           ;; Don't change the word's name token if it has one
           (let ((nt (word-name-token word)))
             (setf (gethash nt nt-to-word-map) word)
             (when (> nt next-nt)
               (setf next-nt (+ nt +cell-size+)))))
          (t
           (setf (word-name-token word) next-nt
                 (gethash next-nt nt-to-word-map) word)
           (incf next-nt +cell-size+)))
    (map nil #'(lambda (marker) (add-word-to-marker marker word)) markers)))

(defmethod note-new-included-file ((wls word-lists) truename)
  (with-slots (markers) wls
    (map nil #'(lambda (marker) (add-included-file-to-marker marker truename)) markers)))

(defmethod save-marker-to-template ((wls word-lists) (marker marker))
  (with-slots (search-order compilation-word-list words included-files) marker
    (list (map 'list #'dictionary-name search-order)
          (dictionary-name compilation-word-list)
          (copy-list words)
          (copy-list included-files))))

(defmethod load-marker-from-template ((wls word-lists) template)
  (destructuring-bind (template-search-order template-compilation-word-list template-words template-included-files) template
    (let* ((search-order (loop for wl in template-search-order
                               collect (word-list wls wl)))
           (compilation-word-list (word-list wls template-compilation-word-list))
           (marker (make-instance 'marker :search-order search-order :compilation-word-list compilation-word-list)))
      (dolist (word (reverse template-words))
        (add-word-to-marker marker word))
      (dolist (truename (reverse template-included-files))
        (add-included-file-to-marker marker truename))
      marker)))
