(in-package #:cl-user)

(defpackage #:aoc-2018-25
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:split-sequence))

(in-package #:aoc-2018-25)

(defun read-points-from-file ()
  (with-open-file (in "25")
    (read-points in)))

(defun read-points-from-string (string)
  (with-input-from-string (in string)
    (read-points in)))

(defun read-points (stream)
  (coerce (loop :for line := (read-line stream nil)
                :while line
                :collect (map 'vector
                              #'parse-integer
                              (split-sequence #\, line)))
          'vector))

(defun manhattan-distance (a b)
  (loop :for x :across a
        :and y :across b
        :sum (abs (- x y))))

(defun connectedp (a b)
  (<= (manhattan-distance a b) 3))

(defun adjacencies (points)
  (map-into (make-array (length points))
            (lambda (point i)
              (loop :for other :across points
                    :for j :upfrom 0
                    :when (and (/= i j)
                               (connectedp point other))
                      :sum (ash 1 j)))
            points
            (iota (length points))))

(defun aoc25a (&optional (points (read-points-from-file))
               &aux (length (length points)))
  (let ((adjacencies (adjacencies points))
        (marks (make-array length :initial-element nil))
        (done 0))
    #+debug
    (print (list :adjacencies (map 'list #'binfmt adjacencies)))
    (loop :for constellation :upfrom 0 :below length
          :when (not (logbitp constellation done))
            :do (loop :for queue := (make-queue constellation)
                        :then (logand (logior queue
                                              (aref adjacencies current))
                                      (lognot done))
                      :for current := (peek-queue queue)
                      :until (= current -1)
                      :do #+debug
                          (print (list :marks marks
                                       :done (binfmt done)
                                       :constellation constellation
                                       :queue (binfmt queue)
                                       :current current))
                          (setf (aref marks current) constellation
                                done (dpb 1 (byte 1 current) done))))
    (hash-table-count (frequencies marks))))

(defun make-queue (&rest ns)
  (loop :for n :in ns
        :sum (ash 1 n)))

(defun peek-queue (n)
  "The index of the rightmost set bit."
  (1- (integer-length (logand n (- n)))))

(defun binfmt (b)
  (format nil "~8,'0b" b))
