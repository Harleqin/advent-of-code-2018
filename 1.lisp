(in-package #:aoc-2018)


(defun aoc1a (&optional (list (read-integers "1")))
  (reduce #'+ list))

(defun aoc1b (&optional (changes (apply #'circular-list (read-integers "1"))))
  (loop :with seen := (make-hash-table)
        :for change :in changes
        :summing change :into f
        :thereis (gethash f seen)
        :do (setf (gethash f seen) f)))
