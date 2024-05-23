(in-package #:forth)

(defclass ffi ()
  ((foreign-space :accessor ffi-foreign-space
                  :initform (make-instance 'data-space :size +cell-size+)))
  )

(defmethod save-to-template ((ffi ffi))
  nil)

(defmethod load-from-template ((ffi ffi) template)
  (declare (ignore template))
  nil)
