(in-package #:cl-user)

(defpackage #:aoc-2018-11
  (:use #:cl
        #:aoc-2018
        #:arrows))

(in-package #:aoc-2018-11)

(defparameter *grid-serial-number* 6548)

(defun aoc11a ()
  (let ((grid (make-grid))
        (best-square nil)
        (best-power -50))
    (dotimes (x 297)
      (dotimes (y 297)
        (let ((square-power (square-power grid x y)))
          (if (> square-power best-power)
              (setf best-square (list (1+ x) (1+ y))
                    best-power square-power)))))
    (format nil "~{~a,~a~}" best-square)))

(defun make-grid ()
  (let ((grid (make-array '(300 300))))
    (dotimes (x 300)
      (dotimes (y 300)
        (setf (aref grid x y) (power-level (1+ x) (1+ y)))))
    grid))

(defun power-level (x y &optional (serial-number *grid-serial-number*))
  (let ((rack-id (+ x 10)))
    (-> rack-id
        (* y)
        (+ serial-number)
        (* rack-id)
        (floor 100)
        (mod 10)
        (- 5))))

(defun square-power (grid x y &optional (size 3))
  (loop :for i :from x
        :repeat size
        :sum (loop :for j :from y
                   :repeat size
                   :sum (aref grid i j))))

(defun aoc11b ()
  (let ((grid (make-grid))
        (best-square nil)
        (best-power (* -5 300 300)))
    (dotimes (x 300)
      (dotimes (y 300)
        (loop :for size :from 1 :to (min (- 300 x) (- 300 y))
              :for square-power := (aref grid x y)
                :then (add-power square-power grid x y size)
              :do (if (> square-power best-power)
                      (setf best-square (list (1+ x) (1+ y) size)
                            best-power square-power)))))
    (format nil "~{~a,~a,~a~}" best-square)))

(defun add-power (power grid x y size)
  (+ power
     (loop :for i :from x :repeat size
           :sum (aref grid i (+ y size -1)))
     (loop :for j :from y :repeat (1- size)
           :sum (aref grid (+ x size -1) j))))
