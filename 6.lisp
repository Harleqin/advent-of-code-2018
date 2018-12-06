(in-package #:aoc-2018)

(defstruct cell
  id
  distance)

(defun read-coordinates-6 ()
  (with-open-file (in "6")
    (loop :for line := (read-line in nil)
          :while line
          :collect (parse-coord line))))

(defun parse-coord (string)
  (->> (split-sequence #\, string)
       (mapcar (curry #'string-trim " "))
       (mapcar #'parse-integer)))

(defun aoc6a (&optional (coordinates (read-coordinates-6)) debug)
  (let* ((extent (grid-extent coordinates))
         (grid (make-array extent))
         (grid-1d (make-array (apply #'* extent)
                              :displaced-to grid)))
    (map-into grid-1d #'make-cell)
    (loop :for coord :in coordinates
          :for id :upfrom 0
          :do (mark-grid grid id coord))
    (let* ((infinity-ids (-> (loop :for cell :in (border grid)
                                   :collect (cell-id cell))
                             remove-duplicates))
           (finity-ids (set-difference (iota (length coordinates))
                                       infinity-ids))
           (cell-ids (map 'vector #'cell-id grid-1d)))
      (values (loop :for id :in finity-ids
                    :maximize (count id cell-ids))
              (when debug
                (show-grid grid))))))

(defun grid-extent (coordinates)
  (let ((xs (mapcar #'first coordinates))
        (ys (mapcar #'second coordinates)))
    (list (reduce #'max xs)
          (reduce #'max ys))))

(defun mark-grid (grid id start)
  (dolist (direction '((1 0)
                       (-1 0)
                       (0 1)
                       (0 -1)))
    (mark-quadrant grid id start direction)))

(defun mark-quadrant (grid id start direction)
  (loop :for coord := start :then (coord+ coord direction)
        :for d :upfrom 0
        :while (apply #'array-in-bounds-p grid coord)
        :do (mark-line grid id coord (rotate-right direction) d)
            (mark-line grid id coord (rotate-left direction) d)))

(defun mark-line (grid id start direction distance)
  (loop :repeat (1+ distance)
        :for coord := start :then (coord+ coord direction)
        :for d :upfrom distance
        :while (and (apply #'array-in-bounds-p grid coord)
                    (maybe-mark grid id coord d))))

(defun maybe-mark (grid id coord distance)
  (let ((cell (apply #'aref grid coord)))
    (if (null (cell-distance cell))
        (setf (cell-distance cell) distance
              (cell-id cell) id)
        (case (signum (- distance (cell-distance cell)))
          (-1 (setf (cell-distance cell) distance
                    (cell-id cell) id))
          (0 (unless (and (cell-id cell)
                          (= (cell-id cell) id))
               (setf (cell-id cell) nil))
             t)
          (1 nil)))))

(defun coord+ (a b)
  (mapcar #'+ a b))

(defun border (grid)
  (destructuring-bind (xmax ymax) (array-dimensions grid)
    (append (loop :for y :from 0 :below ymax
                  :collect (aref grid 0 y)
                  :collect (aref grid (1- xmax) y))
            (loop :for x :from 1 :below (1- xmax)
                  :collect (aref grid x 0)
                  :collect (aref grid x (1- ymax))))))

(defun rotate-left (direction)
  (destructuring-bind (x y) direction
    (list (+ (* x 0) (* y -1))
          (+ (* x 1) (* y 0)))))

(defun rotate-right (direction)
  (destructuring-bind (x y) direction
    (list (+ (* x 0) (* y 1))
          (+ (* x -1) (* y 0)))))

(defun show-grid (grid)
  (with-output-to-string (out)
    (terpri out)
    (dotimes (y (array-dimension grid 1))
      (dotimes (x (array-dimension grid 0))
        (let ((cell (aref grid x y)))
          (format out "[~3a ~3a] "
                  (cell-id cell)
                  (cell-distance cell))))
      (terpri out))))

;; 6 times slower
(defun aoc6a* (&optional (coordinates (read-coordinates-6)) debug)
  (let* ((extent (grid-extent coordinates))
         (grid (make-array extent))
         (grid-1d (make-array (apply #'* extent)
                              :displaced-to grid)))
    (dotimes (x (first extent))
      (dotimes (y (second extent))
        (loop :with best-id := nil
              :with best-distance := nil
              :for coord :in coordinates
              :for id :upfrom 0
              :for distance := (manhattan-distance coord (list x y))
              :do (cond ((or (null best-distance)
                             (< distance best-distance))
                         (setf best-id id
                               best-distance distance))
                        ((= distance best-distance)
                         (setf best-id nil)))
              :finally (setf (aref grid x y)
                             (make-cell :id best-id
                                        :distance best-distance)))))
    (let* ((infinity-ids (-> (loop :for cell :in (border grid)
                                   :collect (cell-id cell))
                             remove-duplicates))
           (finity-ids (set-difference (iota (length coordinates))
                                       infinity-ids))
           (cell-ids (map 'vector #'cell-id grid-1d)))
      (values (loop :for id :in finity-ids
                    :maximize (count id cell-ids))
              (when debug
                (show-grid grid))))))

(defun manhattan-distance (a b)
  (->> (mapcar #'- a b)
       (mapcar #'abs)
       (reduce #'+)))

(defun aoc6b (&optional (coordinates (read-coordinates-6)))
  (let* ((extent (grid-extent coordinates)))
    (loop :for x :below (first extent)
          :sum (loop :for y :below (second extent)
                     :count (< (loop :for coord :in coordinates
                                     :sum (manhattan-distance coord (list x y)))
                               10000)))))
