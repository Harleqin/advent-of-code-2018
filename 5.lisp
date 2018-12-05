(in-package #:aoc-2018)

(defun read-polymer (filename)
  (with-open-file (in filename)
    (read-line in)))

(defun aoc5a (&optional (polymer (read-polymer "5")))
  (length (react polymer)))

(defun react (polymer)
  (let ((product (make-array (length polymer)
                             :fill-pointer 0)))
    (loop :for b :across (map 'vector #'char-code polymer)
          :for last := (vector-peek product)
          :do (if (and last
                       (= (abs (- b last)) 32))
                  (vector-pop product)
                  (vector-push b product)))
    product))

(defun vector-peek (v &aux (fp (fill-pointer v)))
  (unless (zerop fp)
    (aref v (1- fp))))

(defun aoc5b (&optional (polymer (read-polymer "5")))
  (loop :for unit-type :in (mapcar #'code-char
                                   (iota 26 :start (char-code #\A)))
        :minimize (length (react (remove unit-type polymer
                                         :test #'char-equal)))))
