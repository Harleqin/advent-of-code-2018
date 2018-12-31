(in-package #:cl-user)

(defpackage #:aoc-2018-23
  (:use #:cl
        #:alexandria
        #:aoc-2018
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

(defgeneric manhattan-distance (a b))

(defmethod manhattan-distance ((a nanobot) (b nanobot))
  (manhattan-distance (nanobot-pos a) (nanobot-pos b)))

(defmethod manhattan-distance ((a vector) (b nanobot))
  (manhattan-distance a (nanobot-pos b)))

(defmethod manhattan-distance ((a nanobot) (b vector))
  (manhattan-distance (nanobot-pos a) b))

(defmethod manhattan-distance ((a vector) (b vector))
  (loop :for i :across a
        :and j :across b
        :sum (abs (- i j))))

;;; Part 2

;; The goal here is to find clusters of bots whose radii all mutually overlap.
;; I see this as a connected subgraph problem.  This is followed by walking to
;; that full overlap region, then walking towards the origin.

(defun aoc23b (&optional (nanobots (read-nanobots-from-file)))
  (let+ ((adjacency-matrix (adjacency-matrix nanobots))
         ((&values &ign) (visualize-adjacency-matrix adjacency-matrix "23adj"))
         ;; Looking at that matrix, there seem to be a few outliers, while most
         ;; bots overlap.  Quantify that:
         (overlap-counts
          (frequencies (map 'vector #'logcount adjacency-matrix)))
         ;; Yeah, we can eliminate everything below 900 (actually 981, but the
         ;; most overlapping outlier has 607).
         ((&values reduced-matrix blacklist)
          (eliminate-outliers adjacency-matrix 900))
         ;; Taking a look.
         ((&values &ign) (visualize-adjacency-matrix reduced-matrix "23noo"))
         ;; Seems to be white.  Only one frequency left:
         (reduced-overlap-counts
          (frequencies (map 'vector #'logcount reduced-matrix)))
         (octahedron-count (-> reduced-overlap-counts
                               hash-table-alist
                               caar))
         ;; Don't need blacklist anymore, so destroy it.
         (bots (loop :for i :upfrom 0
                     :for bot :across nanobots
                     :if (some-> blacklist first (= i))
                       :do (pop blacklist)
                     :else
                       :collect bot))
         ((&values &ign)
          (progn
            (assert (> (hash-table-count overlap-counts) 1))
            (assert (= (hash-table-count reduced-overlap-counts) 1))
            (assert (= (length bots) octahedron-count))))
         ;; The full overlap region must be convex, by the way.
         (some-full-overlap-pos
          (loop :for pos := (weighted-middle bots)
                  :then (step-pos pos
                                  (weighted-middle missing)
                                  (reduce #'max distances))
                :for (missing distances)
                  := (loop :for bot :in bots
                           :for missing-distance
                             := (- (manhattan-distance pos bot)
                                   (nanobot-radius bot))
                           :when (plusp missing-distance)
                             :collect bot :into missing
                             :and :collect missing-distance :into distances
                           :finally (return (list missing distances)))
                :until (endp missing)
                :do (print (list :distances distances
                                 :length (length distances)))
                    (break)
                :finally (return pos))))
    some-full-overlap-pos))

(defun step-pos (start goal distance)
  (pos-round (pos+ start
                   (pos* distance (normalize (pos- goal start))))))

(defun adjacency-matrix (bots)
  (map-into (make-array (length bots)
                        :element-type 'integer)
            (lambda (bot)
              (loop :for other :across bots
                    :for j :upfrom 0
                    :for bit := (if (<= (manhattan-distance bot other)
                                        (+ (nanobot-radius bot)
                                           (nanobot-radius other)))
                                    1
                                    0)
                    :sum (ash bit j)))
            bots))

(defun visualize-adjacency-matrix (m name &aux (size (length m)))
  (let ((image (opticl:make-8-bit-gray-image size size :initial-element 0)))
    (dotimes (row size)
      (dotimes (col size)
        (setf (opticl:pixel image row col)
              (* 255 (ldb (byte 1 col) (aref m row))))))
    (opticl:write-png-file (format nil "/tmp/~a.png" name) image)))

(defun eliminate-outliers (m count)
  (let+ (((&values keep blacklist)
          (loop :for i :from 0
                :for adj :across m
                :if (> (logcount adj) count)
                  :collect adj :into keep
                :else
                  :collect i :into blacklist
                :finally (return (values keep blacklist))))
         (bites (loop :for (start-1 b-end) :on (cons -1 blacklist)
                      :for start := (1+ start-1)
                      :for end := (or b-end 1000)
                      :collect (byte (- end start) start))))
    (values (map 'vector
                 (lambda (adj)
                   (loop :for bite :in bites
                         :for i :upfrom 0
                         :sum (ash (ldb bite adj) (- (byte-position bite) i))))
                 keep)
            blacklist)))

(defun weighted-middle (bots)
  (loop :for bot :in bots
        :for weight := (/ (nanobot-radius bot))
        :for weighted-pos := #1=(pos* weight (nanobot-pos bot))
          :then (pos+ weighted-pos #1#)
        :sum weight :into weight-sum
        :finally (return (pos-round (pos* (/ weight-sum) weighted-pos)))))

(defun pos* (scalar pos)
  (map 'vector (curry #'* scalar) pos))

(defun pos+ (pos-a pos-b)
  (map 'vector #'+ pos-a pos-b))

(defun pos- (pos-a pos-b)
  (map 'vector #'- pos-a pos-b))

(defun pos-round (pos)
  (map 'vector #'round pos))

(defun normalize (pos)
  (let ((length (sqrt (loop :for c :across pos
                            :sum (* c c)))))
    (pos* (/ length) pos)))
