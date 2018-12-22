(in-package #:cl-user)

(defpackage #:aoc-2018-18
  (:use #:cl
        #:alexandria
        #:aoc-2018))

(in-package #:aoc-2018-18)

(defun read-landscape-from-file (filename)
  (with-open-file (in filename)
    (read-landscape in)))

(defun read-landscape (stream)
  (let ((cs (loop :for line := (read-line stream nil)
                  :while line
                  :collect (coerce line 'list))))
    (make-array (list (length cs)
                      (length (first cs)))
                :element-type 'character
                :initial-contents cs)))

(defun neighbour-counts (array y x)
  (frequencies (loop :for ny :from (1- y) :to (1+ y)
                     :append (loop :for nx :from (1- x) :to (1+ x)
                                   :when (and (array-in-bounds-p array ny nx)
                                              (or (/= ny y)
                                                  (/= nx x)))
                                     :collect (aref array ny nx)))))

(defun update (landscape y x)
  (let ((neighbour-counts (neighbour-counts landscape y x)))
    (ecase (aref landscape y x)
      (#\. (if (>= (gethash #\| neighbour-counts 0) 3)
               #\|
               #\.))
      (#\| (if (>= (gethash #\# neighbour-counts 0) 3)
               #\#
               #\|))
      (#\# (if (and (plusp (gethash #\# neighbour-counts 0))
                    (plusp (gethash #\| neighbour-counts 0)))
               #\#
               #\.)))))

(defun aoc18a (&optional (landscape (read-landscape-from-file "18")))
  (destructuring-bind (height width) (array-dimensions landscape)
    (loop :repeat 10
          :for (current next) :on (circular-list landscape
                                                 (copy-array landscape))
          :do (dotimes (y height)
                (dotimes (x width)
                  (setf (aref next y x)
                        (update current y x))))
          :finally (let ((fs (frequencies (array-flat-view next))))
                     (return (* (gethash #\| fs)
                                (gethash #\# fs)))))))
