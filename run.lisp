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

(defun announce-forth (fs asdf-system)
  (let* ((system (asdf:registered-system asdf-system))
         (cl-forth (asdf:registered-system '#:cl-forth))
         (cl-forth-name (asdf:system-long-name cl-forth))
         (cl-forth-version (asdf:component-version cl-forth)))
    (multiple-value-bind (system-name system-version cl-forth-note)
        (if system
            (if (asdf:system-long-name system)
                (values (asdf:system-long-name system) (asdf:component-version system)
                        (and (not (eq system cl-forth)) (format nil "~A ~A" cl-forth-name cl-forth-version)))
                (values cl-forth-name cl-forth-version nil))
            (values cl-forth-name cl-forth-version  nil))
      (format t "~&~A Version ~A~@[ (built on ~A)~]~%Running under ~A ~A~%~@[~A~%~]" system-name system-version cl-forth-note
              (lisp-implementation-type) (lisp-implementation-version) (fs-announce-addendum fs)))))

(defun run (&key (asdf-system '#:cl-forth) template interpret transcript-file)
  (let ((fs (make-forth-system :template template)))
    (flet ((runner ()
             (announce-forth fs asdf-system)
             (forth-toplevel fs :interpret interpret)))
      (if transcript-file
          ;; Don't use WITH-OPEN-FILE as it will close with :ABORT T if the body does not finish cleanly.
          ;; Our client's application always aborts the connection even after sending the BYE word.
          ;; Also, we want the transcript to persist if the Forth process should get a fatal error.
          (with-open-stream (transcript (open transcript-file :direction :output :element-type 'character :if-exists :supersede
                                                              #+CCL :sharing #+CCL  :lock))
            (let* ((timestamped-transcript (make-timestamped-stream transcript))
                   (input-transcript (make-prefixed-stream "IN: " timestamped-transcript))
                   (output-transcript (make-prefixed-stream "OUT: " timestamped-transcript))
                   (*standard-input* (make-echo-stream *standard-input* input-transcript))
                   (*standard-output* (make-broadcast-stream *standard-output* output-transcript)))
              (add-auto-flush-stream transcript)
              (unwind-protect
                   (runner)
                (remove-auto-flush-stream transcript))))
          (runner)))))

(defun run-forth-process (template &key (asdf-system '#:cl-forth) name interpret transcript-file)
  (let* ((system (asdf:registered-system asdf-system))
         (process-name (cond (name)
                             (system (asdf:system-long-name system))
                             (t "CL-Forth"))))
    (multiple-value-bind (remote-input local-output)
        (make-piped-streams :name (format nil "~A stdin" process-name))
      (multiple-value-bind (local-input remote-output)
          (make-piped-streams :name (format nil "~A stdout" process-name))
        (let ((local-io (make-two-way-stream local-input local-output)))
          (process-run-function process-name
                                #'(lambda ()
                                    (unwind-protect
                                         (let ((*standard-input* remote-input)
                                               (*standard-output* remote-output))
                                           (run :asdf-system asdf-system :template template
                                                :interpret interpret :transcript-file transcript-file))
                                      (close remote-input)
                                      (close remote-output)
                                      (close local-io))))
          local-io)))))
