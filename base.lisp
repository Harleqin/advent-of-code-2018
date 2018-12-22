(in-package #:cl-user)

(defpackage #:aoc-2018
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:for
        #:split-sequence)
  (:export #:array-flat-view
           #:dovector
           #:download
           #:frequencies
           #:read-integers))

(in-package #:aoc-2018)

(defun download (day)
  (let* ((url  (format nil
                       "https://adventofcode.com/2018/day/~s/input"
                       day))
         (input (drakma:http-request url
                                     :cookie-jar *aoc-cookies*)))
    (with-open-file (out (format nil "~s" day)
                         :direction :output)
      (write-sequence input out))))

(defun read-integers (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :append (loop :for (i pos) := (multiple-value-list
                                         (parse-integer line
                                                        :start (or pos 0)
                                                        :junk-allowed t))
                        :while i
                        :collect i))))

(defmacro dovector ((var vector &optional return) &body body)
  `(loop :for ,var :across ,vector
         :do (tagbody
                ,@body)
         :finally (return ,return)))

(defun array-flat-view (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array))

(defun frequencies (sequence &key (test #'eql))
  (let ((fs (make-hash-table :test test)))
    (for ((x over sequence))
      (incf (gethash x fs 0)))
    fs))
