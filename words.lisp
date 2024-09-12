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

(defparameter *forth-words-package* (find-package '#:forth-words))


;;; Type Definitions

(defstruct (dictionary (:constructor %make-dictionary) (:print-function %print-dictionary))
  (name "" :type string)
  (wid 0 :type integer)
  (last-ordinal -1 :type integer)
  (words (make-hash-table :test #'equalp) :type hash-table)
  (parent nil))

(defstruct (parameters (:constructor %make-parameters))
  p1 p2 p3 p4)

(defstruct (word (:constructor %make-word) (:print-function %print-word))
  (name nil :type (or string null))
  (previous nil :type (or word null))
  (smudge? nil :type boolean)
  (immediate? nil :type boolean)
  (compile-only? nil :type boolean)
  (inlineable? nil :type boolean)
  (created-word? nil :type boolean)
  (deferring-word? nil :type boolean)
  (code nil)
  (inline-forms nil)
  (parameters (%make-parameters) :type parameters)
  (documentation nil)
  (does> nil :type (or word null))
  (parent nil :type (or dictionary null))
  (execution-token nil)
  (compile-token nil)
  (name-token nil :type (or integer null))
  (ordinal -1 :type integer))

(defstruct (word-lists (:print-function %print-word-lists))
  (all-word-lists (make-hash-table :test #'equalp) :type hash-table)
  (forth-word-list nil :type (or dictionary null))
  (search-order nil :type list)
  (compilation-word-list nil :type (or dictionary null))
  (next-wid (make-address #xFF 0) :type integer)
  (wid-to-word-list-map (make-hash-table) :type hash-table)
  (next-nt (make-address #xFE 0) :type integer)
  (nt-to-word-map (make-hash-table) :type hash-table)
  (saved-word-lists nil)
  (saved-search-order nil)
  (saved-compilation-word-list nil)
  (markers (make-array 0 :fill-pointer 0 :adjustable t))
  (context 0 :type integer)
  (current 0 :type integer)
  (words-created 0 :type integer)
  (object-code-size 0 :type integer))


;;; Dictionaries

(defun %print-dictionary (dict stream depth)
  (declare (ignore depth))
  (print-unreadable-object (dict stream :type t :identity t)
    (write-string (dictionary-name dict) stream)))

(defun make-dictionary (name wid parent)
  (%make-dictionary :name name :wid wid :parent parent))

(defun add-word (dict word &key override silent)
  (declare (type dictionary dict) (type word word) (optimize (speed 3) (safety 0)))
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

(defun delete-word (dict xts word)
  (declare (type dictionary dict) (type word word) (optimize (speed 3) (safety 0)))
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

(defun show-words (dict)
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

(defun search-dictionary (dict name)
  (declare (type dictionary dict) (optimize (speed 3) (safety 0)))
  (gethash name (dictionary-words dict)))

(defun traverse-wordlist (dict function)
  (declare (type dictionary dict) (optimize (speed 3) (safety 0)))
  (maphash #'(lambda (name word)
               (declare (ignore name) (type word word))
               (if (funcall function (word-name-token word))
                   (loop for old = (word-previous word) then (word-previous old)
                         while old
                         do (when (eq (word-parent old) dict)
                              (unless (funcall function (word-name-token old))
                                (return-from traverse-wordlist))))
                   (return-from traverse-wordlist)))
           (dictionary-words dict)))

(defun forget-word (dict xts word)
  (declare (type dictionary dict) (type word word) (optimize (speed 3) (safety 0)))
  (let ((ordinal (word-ordinal word)))
    (maphash #'(lambda (key a-word)
                 (declare (ignore key))
                 (when (> (word-ordinal a-word) ordinal)
                   (delete-word dict xts a-word)))
             (dictionary-words dict)))
  (delete-word dict xts word))


;;; Word Lists and the Search Order

(defvar *predefined-words* (make-hash-table :test #'equalp))

(defun  %print-word-lists (wls stream depth)
  (declare (ignore depth))
  (with-slots (all-word-lists) wls
    (print-unreadable-object (wls stream :type t :identity t)
      (format stream "~D word list~:P" (hash-table-count all-word-lists)))))

(defmacro define-wls-function (name (wls &rest args) &body body)
  (multiple-value-bind (body declarations doc)
      (uiop:parse-body body)
    (declare (ignore doc))
    `(defun ,name (,wls ,@args)
       (declare (optimize (speed 3) (safety 0))
                (type word-lists ,wls))
       ,@declarations
       ,@body)))

(define-wls-function update-psuedo-state-variables (wls)
  (setf (word-lists-context wls) (dictionary-wid (first (word-lists-search-order wls)))
        (word-lists-current wls) (dictionary-wid (word-lists-compilation-word-list wls))))

(define-wls-function install-predefined-words (wls)
  (maphash #'(lambda (key wl-and-word)
               (declare (ignore key))
               (let ((wl (word-list wls (car wl-and-word) :if-not-found :create)))
                 (add-word wl (cdr wl-and-word) :override t :silent t)))
           *predefined-words*)
  (setf (word-lists-words-created wls) 0))

(define-wls-function register-predefined-words (wls execution-tokens here)
  (declare (ignore wls))
  (maphash #'(lambda (key wl-and-word)
               (declare (ignore key))
               (let ((word (cdr wl-and-word)))
                 (register-execution-token execution-tokens word here)
                 (loop for old-word = (word-previous word) then (word-previous old-word)
                       while old-word
                       do (register-execution-token execution-tokens old-word here))))
           *predefined-words*))

(define-wls-function reset-word-lists (wls)
  (clrhash (word-lists-all-word-lists wls))
  (clrhash (word-lists-wid-to-word-list-map wls))
  (clrhash (word-lists-nt-to-word-map wls))
  (when (word-lists-saved-word-lists wls)
    ;; This will create the FORTH word list, all word lists in the search order, the compilation word list,
    ;; and any other word lists that were created but currently not in use
    (dolist (name-and-wid (word-lists-saved-word-lists wls))
      (word-list wls (car name-and-wid) :if-not-found :create :saved-wid (cdr name-and-wid)))
    (setf (word-lists-search-order wls) (loop for wl in (word-lists-saved-search-order wls)
                                              collect (word-list wls wl :if-not-found :create)))
    ;; In case the user just creates an empty word list and sets it as the compilation word list before GILDing
    (setf (word-lists-compilation-word-list wls) (word-list wls (word-lists-saved-compilation-word-list wls)
                                                            :if-not-found :create))
    (update-psuedo-state-variables wls))
  (when (plusp (hash-table-count *predefined-words*))
    ;; Even if SAVED-WORD-LISTS is NIL, this next call will create the FORTH word list at a minimum
    (install-predefined-words wls)
    (when (null (word-lists-saved-word-lists wls))
      (setf (word-lists-forth-word-list wls) (word-list wls "FORTH"))
      (setf (word-lists-search-order wls) (list (word-lists-forth-word-list wls)))
      (setf (word-lists-compilation-word-list wls) (word-lists-forth-word-list wls))
      (update-psuedo-state-variables wls)))
  (setf (word-lists-markers wls) (make-array 0 :fill-pointer 0 :adjustable t)))

(define-wls-function save-word-lists-state (wls)
  (setf (word-lists-saved-word-lists wls) nil)
  (maphash #'(lambda (name dictionary)
               (push (cons name (dictionary-wid dictionary)) (word-lists-saved-word-lists wls)))
           (word-lists-all-word-lists wls))
  (clrhash *predefined-words*)
  (maphash #'(lambda (dictionary-name dictionary)
               (maphash #'(lambda (word-name word)
                            (let ((key (format nil "~A.~A" dictionary-name word-name)))
                              (setf (gethash key *predefined-words*) (cons dictionary-name word))))
                        (dictionary-words dictionary)))
           (word-lists-all-word-lists wls))
  (setf (word-lists-saved-search-order wls) (map 'list #'dictionary-name (word-lists-search-order wls))
        (word-lists-saved-compilation-word-list wls) (dictionary-name (word-lists-compilation-word-list wls))))

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
  (with-slots (all-word-lists forth-word-list search-order compilation-word-list wid-to-word-list-map nt-to-word-map
               markers words-created)
      wls
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
      (setf forth-word-list (word-list wls "FORTH"))
      (setf search-order (loop for wl in template-search-order
                               collect (word-list wls wl)))
      (setf compilation-word-list (word-list wls template-compilation-word-list))
      (let ((n-markers (length template-markers)))
        (setf markers (make-array n-markers :fill-pointer n-markers :adjustable t))
        (dotimes (i n-markers)
          (setf (aref markers i) (load-marker-from-template wls (aref template-markers i)))))
      (setf words-created 0)
      (update-psuedo-state-variables wls)))
  nil)

(define-wls-function word-list (wls name &key (if-not-found :error) (saved-wid nil))
  (let ((name (or name (symbol-name (gensym "WL")))))
    (or (gethash name (word-lists-all-word-lists wls))
        (case if-not-found
          (:create
           (let* ((wid (or saved-wid (word-lists-next-wid wls)))
                  (word-list (make-dictionary name wid wls)))
             (setf (gethash name (word-lists-all-word-lists wls)) word-list
                   (gethash wid (word-lists-wid-to-word-list-map wls)) word-list)
             (if saved-wid
                 (when (> saved-wid (word-lists-next-wid wls))
                   (setf (word-lists-next-wid wls) (+ saved-wid +cell-size+)))
                 (incf (word-lists-next-wid wls) +cell-size+))
             word-list))
          (:error
           (forth-exception :unknown-word-list "Word list ~A does not exist" name))
          (otherwise nil)))))

(define-wls-function lookup-wid (wls wid)
  (declare (type integer wid))
  (or (gethash wid (word-lists-wid-to-word-list-map wls))
      (forth-exception :unknown-word-list "~14,'0X is not a wordlist id" wid)))

(define-wls-function lookup (wls token)
  (declare (type string token))
  (dolist (dict (word-lists-search-order wls) nil)
    (declare (type dictionary dict))
    (do ((word (gethash token (dictionary-words dict)) (word-previous word)))
        ((null word))
      (when (not (word-smudge? word))
        (return-from lookup word)))))

(define-wls-function lookup-nt (wls nt)
  (declare (type integer nt))
  (or (gethash nt (word-lists-nt-to-word-map wls))
      (forth-exception :not-a-name-token "~14,'0X is not a name token" nt)))
        
(define-wls-function also (wls)
  (push (first (word-lists-search-order wls)) (word-lists-search-order wls)))

(define-wls-function definitions (wls)
  (setf (word-lists-compilation-word-list wls) (first (word-lists-search-order wls)))
  (update-psuedo-state-variables wls))

(define-wls-function only (wls)
  (setf (word-lists-search-order wls) (list (word-lists-forth-word-list wls)))
  (update-psuedo-state-variables wls))

(define-wls-function previous (wls)
  (when (< (length (word-lists-search-order wls)) 2)
    (forth-exception :search-order-underflow))
  (pop (word-lists-search-order wls))
  (update-psuedo-state-variables wls))

(define-wls-function vocabulary (wls name)
  (when (word-list wls name :if-not-found nil)
    (forth-exception :duplicate-word-list "~A is the name of an existing word list" name))
  ;; This will create the list as we already verified it doesn't exist
  (word-list wls name :if-not-found :create))

(defmethod replace-top-of-search-order ((wls word-lists) (dict dictionary))
  (setf (first (word-lists-search-order wls)) dict)
  (update-psuedo-state-variables wls))

(defmethod replace-top-of-search-order ((wls word-lists) (wid integer))
  (let ((word-list (gethash wid (word-lists-wid-to-word-list-map wls))))
    (if word-list
        (replace-top-of-search-order wls word-list)
        (forth-exception :unknown-word-list "~14,'0X is not a wordlist id" wid))))

(define-wls-function note-object-code-size (wls word)
  (declare (type word word))
  (incf (word-lists-object-code-size wls) (object-size (word-code word))))


;;; Words

(defun make-parameters (&optional p1 p2 p3 p4)
  (%make-parameters :p1 p1 :p2 p2 :p3 p3 :p4 p4))

(defmethod make-load-form ((parameters parameters) &optional environment)
  (make-load-form-saving-slots parameters :environment environment))

(defmethod %print-word (word stream depth)
  (declare (ignore depth))
  (print-unreadable-object (word stream :type t :identity t)
    (write-string (or (word-name word) "<Anonymous>") stream)))

(defmethod make-load-form ((word word) &optional environment)
  (make-load-form-saving-slots word :environment environment))

(defmacro define-word (name (&key (word-list "FORTH") ((:word forth-name) (symbol-name name))
                                  immediate? compile-only? (inlineable? (not compile-only?)))
                       &body body)
  (let* ((word (gensym))
         (key (format nil "~A.~A" word-list forth-name))
         (documentation nil)
         (body (loop with forms = (copy-list body)
                     while (stringp (car forms))
                     do (push (pop forms) documentation)
                     finally (return forms)))
         (thunk `(named-lambda ,(intern forth-name *forth-words-package*) (fs parameters)
                   (declare (type forth-system fs) (type parameters parameters) (ignorable fs parameters)
                            (optimize (speed 3) (safety 0)))
                   (with-forth-system (fs)
                     ,@body))))
    `(eval-when (:load-toplevel :execute)
       (let ((,word (%make-word :name ,forth-name
                                :immediate? ,immediate?
                                :compile-only? ,compile-only?
                                :inlineable? ,inlineable?
                                :code ,thunk
                                :inline-forms ',(when inlineable?
                                                  (reverse body))
                                :documentation (reverse ',documentation))))
         (setf (gethash ,key *predefined-words*) (cons ,word-list ,word))))))

(defmacro define-state-word (slot &key (word-list "FORTH") ((:word forth-name) (symbol-name slot)) immediate? compile-only?)
  (let ((description (format nil "Place the address of ~A on the stack" forth-name)))
    `(define-word ,slot (:word-list ,word-list :word ,forth-name :immediate? ,immediate? :compile-only? ,compile-only?)
       "( -- a-addr )"
       ,description
       (stack-push data-stack (state-slot-address memory ',slot)))))

(defun make-word (name code &key smudge? immediate? compile-only? created-word? deferring-word? parameters)
  (%make-word :name name
              :code code
              :smudge? smudge?
              :immediate? immediate?
              :compile-only? compile-only?
              :created-word? created-word?
              :deferring-word? deferring-word?
              :parameters (or parameters (make-parameters))))

(defun forget (word xts)
  (declare (type word word) (optimize (speed 3) (safety 0)))
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
  (with-slots (next-nt nt-to-word-map markers words-created) wls
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
    (map nil #'(lambda (marker) (add-word-to-marker marker word)) markers)
    (incf words-created)))

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
