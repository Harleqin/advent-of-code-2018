(in-package #:cl-user)

(defpackage #:aoc-2018-14
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows))

(in-package #:aoc-2018-14)

(defparameter *n* "939601")

(defun aoc14a (&optional (n (parse-integer *n*)))
  (loop :with recipes := (make-init-array (+ n 10)
                                          :scores '(3 7))
        :for e0 := 0 :then (mod (+ e0 (aref recipes e0) 1)
                                (fill-pointer recipes))
        :for e1 := 1 :then (mod (+ e1 (aref recipes e1) 1)
                                (fill-pointer recipes))
        :for next-recipes := (combine (aref recipes e0)
                                      (aref recipes e1))
        :do (dolist (r next-recipes)
              (unless (vector-push r recipes)
                (return-from aoc14a
                  (format nil "~{~a~}"
                          (coerce (subseq recipes n)
                                  'list)))))))

(defun make-init-array (size &key adjustable scores)
  (let ((array (make-array size
                           :element-type '(mod 10)
                           :fill-pointer 0
                           :adjustable adjustable)))
    (dolist (s scores array)
      (vector-push s array))))

(defun combine (a b)
  (let ((c (+ a b)))
    (if (zerop c)
        '(0)
        (split-digits c))))

(defun split-digits (c)
  (reverse (loop :for (n d) := (multiple-value-list (floor c 10))
                   :then (multiple-value-list (floor n 10))
                 :while (or (plusp n) (plusp d))
                 :collect d)))

(defun aoc14b (&optional (n *n*))
  (loop :with recipes := (make-init-array (parse-integer n)
                                          :adjustable t
                                          :scores '(3 7))
        :with target := (map 'vector #'digit-char-p n)
        :with target-length := (length target)
        :for e0 := 0 :then (mod (+ e0 (aref recipes e0) 1)
                                (fill-pointer recipes))
        :for e1 := 1 :then (mod (+ e1 (aref recipes e1) 1)
                                (fill-pointer recipes))
        :for next-recipes := (combine (aref recipes e0)
                                      (aref recipes e1))
        :do (dolist (r next-recipes)
              (vector-push-extend r recipes)
              (when (and (>= (fill-pointer recipes) target-length)
                         (loop :for i :downfrom (1- (fill-pointer recipes))
                               :for j :downfrom (1- target-length) :to 0
                               :always (= (aref recipes i) (aref target j))))
                (return-from aoc14b (- (fill-pointer recipes) target-length))))))
