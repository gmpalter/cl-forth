(in-package #:forth)

(defclass dictionary ()
  ((name :accessor dictionary-name :initarg :name)
   (psuedo-address :accessor dictionary-psuedo-address :initarg :psuedo-address)
   (words :accessor dictionary-words :initform (make-hash-table :test #'equalp)))
  )

(defmethod print-object ((dict dictionary) stream)
  (print-unreadable-object (dict stream :type t :identity t)
    (write-string (dictionary-name dict) stream)))

(defmethod add-word ((dict dictionary) word &key override)
  (with-slots (words) dict
    (let ((old (gethash (word-name word) words)))
      (when (and old (not override))
        (format t "~A isn't unique. " (word-name word))
        (setf (word-previous word) old))
      (setf (gethash (word-name word) words) word))))

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


;;;

(defvar *predefined-words* (make-hash-table :test #'equalp))

(defclass word-lists ()
  ((all-word-lists :initform (make-hash-table :test #'equalp))
   (forth :reader word-lists-forth-word-list :initform nil)
   (search-order :reader  word-lists-search-order :initform nil)
   (compilation-word-list :reader word-lists-compilation-word-list :initform nil)
   (next-psuedo-address :initform (dpb #x55 (byte 8 48) 0))
   (address-to-word-list-map :initform (make-hash-table))
   (context :initform 0)
   (current :initform 0))
  )

(defmethod initialize-instance :after ((wls word-lists) &key &allow-other-keys)
  (with-slots (all-word-lists forth search-order compilation-word-list) wls
    (setf forth (word-list wls "FORTH" :if-not-found :create))
    (setf (gethash "FORTH" all-word-lists) forth)
    (setf search-order (list forth))
    (setf compilation-word-list forth)
    (update-psuedo-state-variables wls)))

(defmethod print-object ((wls word-lists) stream)
  (with-slots (all-word-lists) wls
    (print-unreadable-object (wls stream :type t :identity t)
      (format stream "~D word list~:P" (hash-table-count all-word-lists)))))

(defmethod update-psuedo-state-variables ((wls word-lists))
  (with-slots (search-order compilation-word-list context current) wls
    (setf context (dictionary-psuedo-address (first search-order))
          current (dictionary-psuedo-address compilation-word-list))))

(defmethod install-predefined-words ((wls word-lists))
  (maphash #'(lambda (forth-name wl-and-word)
               (declare (ignore forth-name))
               (let ((wl (word-list wls (car wl-and-word))))
                 (add-word wl (cdr wl-and-word) :override t)))
           *predefined-words*))

(defmethod word-list ((wls word-lists) name &key (if-not-found :error))
  (with-slots (all-word-lists address-to-word-list-map next-psuedo-address) wls
    (or (gethash name all-word-lists)
        (case if-not-found
          (:create
           (let ((word-list (make-instance 'dictionary :name name :psuedo-address next-psuedo-address)))
             (setf (gethash name all-word-lists) word-list
                   (gethash next-psuedo-address address-to-word-list-map) word-list)
             (incf next-psuedo-address +cell-size+)
             word-list))
          (:error
           (forth-exception :unknown-word-list "Word list ~A does not exist" name))
          (otherwise nil)))))

(defmethod lookup ((wls word-lists) token)
  (with-slots (search-order) wls
    (loop for dictionary in search-order
            thereis (let ((word (gethash token (dictionary-words dictionary))))
                      (and word (not (word-smudge? word)) word)))))

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

(defmethod replace-top-of-search-order ((wls word-lists) (psuedo-address integer))
  (with-slots (search-order address-to-word-list-map) wls
    (let ((word-list (gethash psuedo-address address-to-word-list-map)))
      (if word-list
          (replace-top-of-search-order wls word-list)
          (forth-exception :unknown-word-list "~14,'0X is not the address of a word list" psuedo-address)))))


;;;

(defclass word ()
  ((name :accessor word-name :initarg :name)
   (previous :accessor word-previous :initarg :previous :initform nil)
   (smudge? :accessor word-smudge? :initarg :smudge? :initform nil)
   (immediate? :accessor word-immediate? :initarg :immediate? :initform nil)
   (compile-only? :accessor word-compile-only? :initarg :compile-only? :initform nil)
   (inlineable? :accessor word-inlineable? :initarg :inlineable? :initform nil)
   (code :accessor word-code :initarg :code :initform nil)
   (inline-forms :accessor word-inline-forms :initarg :inline-forms :initform nil)
   (parameters :accessor word-parameters :initarg :parameters :initform nil))
  )

(defmethod print-object ((word word) stream)
  (print-unreadable-object (word stream :type t :identity t)
    (write-string (or (word-name word) "<Anonymous>") stream)))

(defmacro define-word (name (&key (word-list "FORTH") ((:word forth-name) (symbol-name name))
                                  immediate? compile-only? (inlineable? t))
                       &body body)
  (let* ((word (gensym))
         (body (loop with forms = (copy-list body)
                     while (stringp (car forms))
                     do (pop forms)
                     finally (return forms)))
         (thunk `(lambda (fs &rest parameters)
                   (declare (ignorable parameters))
                   (with-forth-system (fs)
                     ,@body))))
    `(eval-when (:load-toplevel :execute)
       (let ((,word (make-instance 'word
                                   :name ,forth-name
                                   :immediate? ,immediate?
                                   :compile-only? ,compile-only?
                                   :inlineable? ,inlineable?
                                   :code (compile nil ,thunk)
                                   :inline-forms ',(when inlineable?
                                                     (copy-tree body)))))
         (setf (gethash ,forth-name *predefined-words*) (cons ,word-list ,word))))))

(defmacro define-state-word (slot &key (word-list "FORTH") ((:word forth-name) (symbol-name slot)) immediate? compile-only?)
  (let ((description (format nil "Place the address of ~A on the stack" forth-name)))
    `(define-word ,slot (:word-list ,word-list :word ,forth-name :immediate? ,immediate? :compile-only? ,compile-only?)
       "( - a-addr )"
       ,description
       (stack-push data-stack (state-slot-address memory ',slot)))))

(defun make-word (name code &key smudge? immediate? compile-only? parameters)
  (make-instance 'word :name name
                       :code code
                       :smudge? smudge?
                       :immediate? immediate?
                       :compile-only? compile-only?
                       :parameters (copy-list parameters)))
