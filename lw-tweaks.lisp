(in-package #:forth)

(declaim (inline whitespacep))
(defun whitespacep (ch) (lw:whitespace-char-p ch))
