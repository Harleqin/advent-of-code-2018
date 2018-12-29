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
  (bots 0 :type integer)
  (candidates 0 :type integer))

(defun aoc23b (&optional (*nanobots* (read-nanobots-from-file)))
  (let* ((*adjacency-matrix* (adjacency-matrix *nanobots*))
         ;; A cluster is represented as a bit field of nanobots in this cluster.
         #+debug
         (largest-clusters
           (loop :for clusters
                   := (loop :for i :below (length *nanobots*)
                            :collect (create-cluster (ash 1 i)))
                     :then new-clusters
                 :for new-clusters := (mapcan #'extend-cluster clusters)
                 :while new-clusters
                 :finally (return clusters))))
    (visualize-adjacency-matrix *adjacency-matrix*)))

(defun visualize-adjacency-matrix (m)
  (let ((image (opticl:make-8-bit-gray-image 1000 1000 :initial-element 0)))
    (dotimes (row 1000)
      (dotimes (col 1000)
        (setf (opticl:pixel image row col)
              (* 255 (ldb (byte 1 col) (aref m row))))))
    (opticl:write-png-file "/tmp/23.png" image)))

(defun extend-cluster (cluster)
  (let+ (((&structure cluster- bots candidates) cluster))
    (loop :for i :below (integer-length candidates)
          :for next-candidates
            := (when (logbitp i candidates)
                 (logand candidates
                         (aref *adjacency-matrix* i)))
          :when next-candidates
            :collect (make-cluster :bots (logior bots (ash 1 i))
                                   :candidates next-candidates))))

(defun create-cluster (bot-indicator)
  (make-cluster :bots bot-indicator
                :candidates (candidate-mask bot-indicator)))

(defun candidate-mask (bots-bitfield)
  "Returns a bitfield integer indicating every bot not indicated by
BOTS-BITFIELD whose radius overlaps every bot in BOTS-BITFIELD."
  ;; TODO: das geht schneller -> nur die 1en ansehen
  (let ((adjacencies (loop :for i :below (integer-length bots-bitfield)
                           :when (logbitp i bots-bitfield)
                             :collect (aref *adjacency-matrix* i))))
    (reduce #'logand adjacencies)))

(defun adjacency-matrix (bots)
  (map-into (make-array (length bots)
                        :element-type 'integer)
            (lambda (bot i)
              (loop :for other :across bots
                    :for j :upfrom 0
                    :for bit := (if (and (/= i j)
                                         (<= (manhattan-distance bot other)
                                             (+ (nanobot-radius bot)
                                                (nanobot-radius other))))
                                    1
                                    0)
                    :sum (ash bit j)))
            bots
            (iota (length bots))))
