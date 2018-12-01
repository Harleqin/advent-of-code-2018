(in-package #:aoc-2018)

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

(defun aoc1a (&optional (list (read-integers "1")))
  (reduce #'+ list))

(defun aoc1b (&optional (changes (apply #'circular-list (read-integers "1"))))
  (loop :with seen := (make-hash-table)
        :for change :in changes
        :summing change :into f
        :thereis (gethash f seen)
        :do (setf (gethash f seen) f)))
