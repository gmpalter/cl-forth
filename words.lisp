(in-package #:forth)

(defclass dictionary ()
  ((name :accessor dictionary-name :initarg :name)
   (words :accessor dictionary-words :initform (make-hash-table :test #'equalp)))
  )

(defmethod add-word ((dict dictionary) word &key override)
  (with-slots (words) dict
    (let ((old (gethash (word-name word) words)))
      (when (and old (not override))
        (format t "~A isn't unique. " (word-name word))
        (setf (word-previous word) old))
      (setf (gethash (word-name word) words) word))))


;;;

(defvar *predefined-words* (make-hash-table :test #'equalp))

(defclass word-lists ()
  ((all-word-lists :initform (make-hash-table :test #'equalp))
   (forth :initform (make-instance 'dictionary :name "Forth"))
   (search-order :accessor word-lists-search-order :initform nil)
   (compilation-word-list :accessor word-lists-compilation-word-list :initform nil)
   (context :initform 0)
   (current :initform 0))
  )

(defmethod initialize-instance :after ((wls word-lists) &key &allow-other-keys)
  (with-slots (all-word-lists forth search-order compilation-word-list) wls
    (setf (gethash "Forth" all-word-lists) forth)
    (setf search-order (list forth))
    (setf compilation-word-list forth)
    (update-psuedo-state-variables wls)))

(defmethod update-psuedo-state-variables ((wls word-lists))
  (with-slots (search-order compilation-word-list context current) wls
    (setf context (%address-of (first search-order))
          current (%address-of compilation-word-list))))

(defmethod install-predefined-words ((wls word-lists))
  (maphash #'(lambda (forth-name wl-and-word)
               (declare (ignore forth-name))
               (let ((wl (word-list wls (car wl-and-word))))
                 (add-word wl (cdr wl-and-word) :override t)))
           *predefined-words*))

(defmethod word-list ((wls word-lists) name &key (if-not-found :error))
  (with-slots (all-word-lists) wls
    (or (gethash name all-word-lists)
        (case if-not-found
          (:create
           (setf (gethash name all-word-lists) (make-instance 'dictionary :name name)))
          (:error
           (forth-error :unknown-word-list "Word list ~A does not exist" name))
          (otherwise nil)))))

(defmethod lookup ((wls word-lists) token)
  (with-slots (search-order) wls
    (loop for dictionary in search-order
            thereis (let ((word (gethash token (dictionary-words dictionary))))
                      (and word (not (word-smudge? word)) word)))))

;;; ALSO
;;; ASSEMBLER ??
;;; DEFINITIONS
;;; EDITOR ??
;;; FORTH
;;; ONLY
;;; ORDER
;;; PREVIOUS
;;; VOCABULARY
;;; WORDS


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

(defmacro define-word (name (&key (word-list "Forth") ((:word forth-name) (symbol-name name))
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

(defmacro define-state-word (slot &key (word-list "Forth") ((:word forth-name) (symbol-name slot)) immediate? compile-only?)
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

