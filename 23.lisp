(in-package #:cl-user)

(defpackage #:aoc-2018-23
  (:use #:cl
        #:alexandria
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2018-23)

(defun read-nanobots-from-file ()
  (with-open-file (in "23")
    (read-nanobots in)))

(defun read-nanobots (in)
  (coerce (loop :for line := (read-line in nil)
                :while line
                :collect (parse-nanobot line))
          'vector))

(defstruct nanobot
  (pos #(0 0 0) :type (vector integer 3))
  (radius 0 :type (integer 0)))

(defun parse-nanobot (string)
  (or (register-groups-bind ((#'parse-integer x y z r))
          ("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)" string)
        (make-nanobot :pos (vector z y x)
                      :radius r))
      (error "line not parsable: ~s" string)))

(defun aoc23a (&optional (nanobots (read-nanobots-from-file)))
  (let ((strongest (reduce (lambda (a b)
                             (if (> (nanobot-radius a)
                                    (nanobot-radius b))
                                 a b))
                           nanobots)))
    (loop :for bot :across nanobots
          :count (<= (manhattan-distance strongest bot)
                     (nanobot-radius strongest)))))

(defun manhattan-distance (a b)
  (declare (nanobot a b))
  (loop :for i :across (nanobot-pos a)
        :and j :across (nanobot-pos b)
        :sum (abs (- i j))))

;;; Part 2

;; The goal here is to find clusters of bots whose radii all mutually overlap.
;; I see this as a connected subgraph problem.

(defvar *nanobots*)
(defvar *adjacency-matrix*)

(defstruct cluster
  bots
  candidates)

(defun aoc23b (&optional (*nanobots* (read-nanobots-from-file)))
  (let ((*adjacency-matrix* (adjacency-matrix *nanobots*)))
    ;; A cluster is represented as a bit field of nanobots in this cluster.
    (loop :for clusters := (loop :for i :below (length *nanobots*)
                                 :collect (ash 1 i))
            :then new-clusters
          :for new-clusters := (mapcan #'extend-cluster clusters)
          :while new-clusters
          :finally (return clusters))))

(defun extend-cluster (cluster)
  )

;; TODO: make this a vector of bitfield integers
(defun adjacency-matrix (bots)
  (let ((m (make-array (list (length bots) (length bots))
                       :element-type 'bit
                       :initial-element 0)))
    (loop :for a :across bots
          :for i :upfrom 0
          :do (loop :for b :across bots
                    :for j :upfrom 0
                    :do (when (and (/= i j)
                                   (<= (manhattan-distance a b)
                                       (+ (nanobot-radius a)
                                          (nanobot-radius b))))
                          (setf (aref m i j) 1))))))
