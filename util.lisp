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
