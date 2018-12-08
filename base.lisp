(in-package #:cl-user)

(defpackage #:aoc-2018
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:split-sequence)
  (:export #:download
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
