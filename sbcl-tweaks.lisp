(in-package #:forth)

(declaim (inline whitespacep))
(defun whitespacep (ch) (sb-unicode:whitespace-p))
