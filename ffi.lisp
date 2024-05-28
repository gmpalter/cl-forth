(in-package #:forth)

(defclass foreign-space (mspace)
  ()
  )

(defmethod print-object ((sp foreign-space) stream)
  (with-slots (prefix) sp
    (print-unreadable-object (sp stream :type t :identity t)
      (format stream "prefix=~2,'0X" prefix))))

(defmethod space-reset ((sp foreign-space))
  nil)

(defmethod save-space-state ((sp foreign-space))
  nil)

;;;---*** TODO: Is there anything to save?
(defmethod space-save-to-template ((sp foreign-space))
  nil)

;;;---*** TODO: Is there anything to load?
(defmethod space-load-from-template ((sp foreign-space) template)
  (declare (ignore template))
  nil)

(defmethod space-allocate ((sp foreign-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :invalid-memory))

(defmethod space-deallocate ((sp foreign-space) n-bytes)
  (declare (ignore n-bytes))
  (forth-exception :invalid-memory))

(defmethod space-unused ((sp foreign-space))
  0)

(defmethod space-align ((sp foreign-space) &optional (boundary +cell-size+))
  (declare (ignore boundary))
  nil)

(defmethod cell-at ((sp foreign-space) address)
  (cffi:mem-ref (address-pointer address) :int64))

(defmethod (setf cell-at) (value (sp foreign-space) address)
  (setf (cffi:mem-ref (address-pointer address) :int64) value))

(defmethod cell-unsigned-at ((sp foreign-space) address)
  (cffi:mem-ref (address-pointer address) :uint64))

(defmethod (setf cell-unsigned-at) (value (sp foreign-space) address)
  (setf (cffi:mem-ref (address-pointer address) :uint64) value))

(defmethod quad-byte-at ((sp foreign-space) address)
  (cffi:mem-ref (address-pointer address) :uint32))

(defmethod (setf quad-byte-at) (value (sp foreign-space) address)
  (setf (cffi:mem-ref (address-pointer address) :uint32) value))

(defmethod double-byte-at ((sp foreign-space) address)
  (cffi:mem-ref (address-pointer address) :uint16))

(defmethod (setf double-byte-at) (value (sp foreign-space) address)
  (setf (cffi:mem-ref (address-pointer address) :uint16) value))

(defmethod byte-at ((sp foreign-space) address)
  (cffi:mem-ref (address-pointer address) :uint8))

(defmethod (setf byte-at) (value (sp foreign-space) address)
  (setf (cffi:mem-ref (address-pointer address) :uint8) value))

(defmethod space-decode-address ((sp foreign-space) address &optional size-hint)
  (let* ((offset (mod address +cell-size+))
         (address (- address offset))
         (pointer (cffi:make-pointer address))
         (size (+ (or size-hint (expt 2 15)) offset)))
    (values (cffi:foreign-array-to-lisp pointer `(:array :uint8 ,size) :element-type '(unsigned-byte 8))
            offset
            size)))

(defmethod space-native-address ((sp foreign-space) foreign-address)
  ;;---*** TODO: What if the FOREIGN-ADDRESS has a non-zero PREFIX?
  (with-slots (prefix) sp
    (make-address prefix foreign-address)))

(defmethod space-foreign-address ((sp foreign-space) native-address)
  native-address)

(defmethod space-address-is-foreign? ((sp foreign-space) address)
  (declare (ignore address))
  t)

;;; NOTE: We have no way to bounds check this operation ...
(defmethod space-fill ((sp foreign-space) address count byte)
  (cffi:foreign-funcall "memset" :pointer (address-pointer address) :uint8 byte :size count :pointer)
  nil)

;;; NOTE: We have no way to bounds check this operation ...
(defmethod space-copy ((ssp foreign-space) source-address (dsp foreign-space) destination-address count)
  (cffi:foreign-funcall "memcpy" :pointer (address-pointer destination-address) :pointer (address-pointer source-address)
                                 :size count :pointer)
  nil)

;;; NOTE: We have no way to bounds check the foreign space in this operation ...
(defmethod space-copy ((ssp foreign-space) source-address (dsp mspace) destination-address count)
  (multiple-value-bind (destination-data destination-address destination-size)
      (space-decode-address dsp destination-address count)
    (unless (<= (+ destination-address count) destination-size)
      (forth-exception :invalid-memory))
    (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer (%address-of destination-data) destination-address)
                                   :pointer (address-pointer source-address)
                                   :size count :pointer)
    nil))

;;; NOTE: We have no way to bounds check the foreign space in this operation ...
(defmethod space-copy ((ssp mspace) source-address (dsp foreign-space) destination-address count)
  (multiple-value-bind (source-data source-address source-size)
      (space-decode-address ssp source-address count)
    (unless (<= (+ source-address count) source-size)
      (forth-exception :invalid-memory))
    (cffi:foreign-funcall "memcpy" :pointer (address-pointer destination-address)
                                   :pointer (cffi:inc-pointer (%address-of source-data) source-address)
                                   :size count :pointer)
    nil))


;;;

(defstruct library
  name
  ffi-library)

(defclass ffi ()
  ((foreign-space :accessor ffi-foreign-space :initform (make-instance 'foreign-space))
   (libraries :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (current-library :accessor ffi-current-library :initform nil))
  )

;;;---*** TODO: Save foreign libraries?
(defmethod save-to-template ((ffi ffi))
  nil)

;;;---*** TODO: Reload foreign libraries?
(defmethod load-from-template ((ffi ffi) template)
  (declare (ignore template))
  nil)

(defmethod load-foreign-library ((ffi ffi) name-or-path)
  (with-slots (libraries current-library) ffi
    (handler-case
        (let* ((library-symbol (intern (string-upcase (file-namestring name-or-path)) '#:forth-ffi-symbols))
               (name-or-path (if (string-equal (file-namestring name-or-path) name-or-path)
                                 name-or-path
                                 (pathname name-or-path)))
               (library (make-library :name name-or-path :ffi-library library-symbol)))
          ;; If REGISTER-FOREIGN-LIBRARY were exported, I'd use it instead to avoid using EVAL
          (eval `(cffi:define-foreign-library ,library-symbol (t ,name-or-path)))
          (cffi:load-foreign-library library-symbol)
          (when (null (position library-symbol libraries :key #'library-ffi-library))
            (vector-push-extend library libraries))
          (setf current-library library))
      (cffi:load-foreign-library-error (e)
        (forth-exception :cant-load-foreign-library "~A" e)))))

(defmethod build-ffi-call ((ffi ffi) name library parameters return-value)
  (let* ((lambda-name (intern (string-upcase name) '#:forth-ffi-symbols))
         (parameter-symbols (mapcar #'(lambda (x) (declare (ignore x)) (gensym "PARM")) parameters))
         (result-symbol (gensym "RESULT"))
         (parameter-forms
           (loop for parameter in parameters
                 for symbol in parameter-symbols
                 collect `(,symbol ,@(case parameter
                                       (:int64
                                        `((cell-signed (stack-pop data-stack))))
                                       (:uint64
                                        `((cell-unsigned (stack-pop data-stack))))
                                       (:int32
                                        `((quad-byte-signed (stack-pop data-stack))))
                                       (:uint32
                                        `((quad-byte-unsigned (stack-pop data-stack))))
                                       (:pointer
                                        `((foreign-pointer memory (stack-pop data-stack))))
                                       (:single
                                        `((>single-float (stack-pop float-stack))))
                                       (:double
                                        `((>double-float (stack-pop float-stack))))))))
         (call-form
           `((,result-symbol (cffi:foreign-funcall (,name :library ,(library-ffi-library library))
                                                   ,@(loop for parameter in parameters
                                                           for symbol in parameter-symbols
                                                           collect parameter
                                                           collect symbol)
                                                   ,return-value))))
         (return-form
           (case return-value
             (:void
              nil)
             ((:int64 :uint64 :int32 :uint32)
              `((stack-push data-stack ,result-symbol)))
             (:pointer
              `((stack-push data-stack (native-address memory ,result-symbol))))
             (:single
              `((stack-push float-stack (native-float ,result-symbol))))
             (:double
              `((stack-push float-stack (native-float ,result-symbol))))))
         (thunk `(named-lambda ,lambda-name (fs &rest parameters)
                   (declare (ignorable parameters))
                   (with-forth-system (fs)
                     (let* (,@(reverse parameter-forms)
                            ,@call-form)
                       (declare (ignorable ,result-symbol))
                       ,@return-form)))))
    (compile nil (eval thunk))))

(defmethod build-ffi-callback ((ffi ffi) fs name xt parameters return-value)
  (let* ((callback (intern name '#:forth-ffi-symbols))
         (parameter-symbols (mapcar #'(lambda (x) (declare (ignore x)) (gensym "PARM")) parameters)))
    (eval
     `(cffi:defcallback ,callback ,return-value (,@(loop for parameter in parameters
                                                         for parameter-symbol in parameter-symbols
                                                         collect `(,parameter-symbol ,parameter)))
        (funcall #'(lambda (fs xt)
                     (with-forth-system (fs)
                       (unwind-protect
                            (progn
                              (save-stack data-stack)
                              (save-stack float-stack)
                              ,@(loop for parameter in parameters
                                      for parameter-symbol in parameter-symbols
                                      collect (case parameter
                                                ((:int64 :uint64 :int32 :uint32)
                                                 `(stack-push data-stack ,parameter-symbol))
                                                (:pointer
                                                 `(stack-push data-stack (native-address memory ,parameter-symbol)))
                                                (:single
                                                 `(stack-push float-stack (native-float ,parameter-symbol)))
                                                (:double
                                                 `(stack-push float-stack (native-float ,parameter-symbol)))))
                              (execute execution-tokens xt fs)
                              ,@(case return-value
                                  (:void nil)
                                  (:int64
                                   `((cell-signed (stack-pop data-stack))))
                                  (:uint64
                                   `((cell-unsigned (stack-pop data-stack))))
                                  (:int32
                                   `((quad-byte-signed (stack-pop data-stack))))
                                  (:uint32
                                   `((quad-byte-unsigned (stack-pop data-stack))))
                                  (:pointer
                                   `((foreign-pointer memory (stack-pop data-stack))))
                                  (:single
                                   `((>single-float (stack-pop float-stack))))
                                  (:double
                                   `((>double-float (stack-pop float-stack)))))))
                       (restore-stack data-stack)
                       (restore-stack float-stack)))
                 ,fs ,xt)))))
  
