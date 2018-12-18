(in-package #:cl-user)

(defpackage #:aoc-2018-15
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2018-15)

(defstruct unit
  id
  type
  hit-points
  attack-power
  coord)

(defstruct path
  steps
  to)

(defun load-map ()
  (read-file-into-string "15"))

(defun parse-map (string)
  (let ((*id-counter* 0))
    (with-input-from-string (in string)
      (let* ((lines (loop :for line := (read-line in nil)
                          :while (plusp (length line))
                          :collect line))
             (size (list (length lines) (length (first lines))))
             (map (make-array size))
             (units (make-array 10
                                :adjustable t
                                :fill-pointer 0)))
        (loop :for line :in lines
              :for y :upfrom 0
              :do (loop :for c :across line
                        :for x :upfrom 0
                        :for cell :=  (parse-cell c (list y x))
                        :do (setf (aref map y x) cell)
                            (when (unit-p cell)
                              (vector-push-extend cell units))))
        (values map units)))))

(defun parse-cell (character coord)
  (ecase character
    (#\# :wall)
    (#\. :free)
    ((#\E #\G) (parse-unit character coord))))

(defvar *id-counter*)

(defvar *elf-power* 3)

(defun parse-unit (character coord)
  (make-unit :id (prog1 *id-counter* (incf *id-counter*))
             :type character
             :hit-points 200
             :attack-power (ecase character
                             (#\E *elf-power*)
                             (#\G 3))
             :coord coord))

(defun reading-order (array coord)
  (+ (* (array-dimension array 1)
        (first coord))
     (second coord)))

(defvar *map*)

(defvar *units*)

(defun aoc15a (&key
                 (raw-map (load-map)))
  (multiple-value-bind (*map* *units*) (parse-map raw-map)
    (loop :for round :upfrom 0
          :do (setf *units* (sort *units* #'<
                                  :key (compose (curry #'reading-order
                                                       *map*)
                                                #'unit-coord)))
              (handler-case
                  (execute-round)
                (end-of-combat ()
                  (let ((remaining-hit-points
                          (reduce #'+
                                  (map 'vector
                                       #'unit-hit-points
                                       *units*))))
                    (print-map *map*)
                    (return-from aoc15a
                      (values (* round remaining-hit-points)
                              round
                              remaining-hit-points))))))))

(defun execute-round ()
  (dovector (unit *units*)
    (take-turn unit)))

(defun print-map (map &key (stream *standard-output*) omit-hit-points)
  (dotimes (y (array-dimension map 0))
    (let ((hit-points ()))
      (dotimes (x (array-dimension map 1))
        (let ((cell (aref map y x)))
          (princ (cond ((eq cell :free) #\.)
                       ((eq cell :wall) #\#)
                       ((unit-p cell)
                        (push (list (unit-type cell) (unit-hit-points cell))
                              hit-points)
                        (unit-type cell))
                       (t #\?))
                 stream)))
      (unless omit-hit-points
        (dolist (hp (reverse hit-points))
          (princ #\space stream)
          (princ hp stream))))
    (terpri stream)))

(defun take-turn (unit)
  "UNIT takes its turn given UNITS in reading order and MAP."
  (when (plusp (unit-hit-points unit))
    (let ((targets (find-targets unit)))
      (when (emptyp targets)
        (signal 'end-of-combat))
      (when (emptyp #1=(remove-if-not (curry #'adjacentp unit)
                                      targets))
        (move unit targets))
      (let ((adjacent-targets #1#))       ; calculate again
        (unless (emptyp adjacent-targets)
          (attack unit
                  (reduce #'better-target adjacent-targets)))))))

(defun better-target (a b)
  (let ((a-hit-points (unit-hit-points a))
        (b-hit-points (unit-hit-points b)))
    (cond ((< a-hit-points b-hit-points)
           a)
          ((> a-hit-points b-hit-points)
           b)
          ((< (reading-order *map* (unit-coord a))
              (reading-order *map* (unit-coord b)))
           a)
          (t b))))

(defun adjacentp (unit-a unit-b)
  (= 1 (manhattan-distance (unit-coord unit-a)
                           (unit-coord unit-b))))

(define-condition end-of-combat () ())

(defun find-targets (unit)
  (remove-if (lambda (other)
               (eq (unit-type other) (unit-type unit)))
             *units*))

(defun move (unit targets)
  (let* ((from-coord (unit-coord unit))
         (target-spaces (find-spaces targets))
         (distances (mapcar (lambda (space)
                              (shortest-path-length from-coord
                                                    space))
                            target-spaces))
         (nearest-target (or (min-with target-spaces distances)
                             (return-from move nil)))
         (free-adjacent-spaces
           (->> +directions+
                (mapcar (lambda (dir)
                          (mapcar #'+ from-coord dir)))
                (remove-if-not (lambda (coord)
                                 (eq :free (apply #'aref *map* coord))))))
         (fas-distances (mapcar (lambda (space)
                                  (shortest-path-length space
                                                        nearest-target))
                                free-adjacent-spaces))
         (to-coord (min-with free-adjacent-spaces
                             fas-distances)))
    (when to-coord
      (setf (unit-coord unit) to-coord
            (apply #'aref *map* from-coord) :free
            (apply #'aref *map* to-coord) unit))))

(defun min-with (items metrics)
  "Returns the first of those of ITEMS that have minimum non-nil corresponding
metric from METRICS."
  (loop :for item :in items
        :for metric :in metrics
        :for (best best-metric) := (list item metric)
          :then (if (or (null best-metric)
                        (and metric
                             (< metric best-metric)))
                    (list item metric)
                    (list best best-metric))
        :finally (return (when best-metric best))))


(defun find-spaces (targets)
  (sort (loop :for target :across targets
              :append (free-neighbours (unit-coord target)))
        #'<
        :key (curry #'reading-order *map*)))

(define-constant +directions+
    '((-1 0)
      (0 -1)
      (0 1)
      (1 0))
  :test #'equal
  :documentation "Directions (up, left, right, down) in reading order.")

(defun free-neighbours (coord)
  (loop :for d :in +directions+
        :for neighbour := (mapcar #'+ coord d)
        :when (eq (apply #'aref *map* neighbour) :free)
          :collect neighbour))

(defun shortest-path-length (from to)
  (if (equal from to)
      0
      (let ((lengths-array (make-array (array-dimensions *map*)
                                       :initial-element most-positive-fixnum)))
        (flet ((heuristic (coord)
                 (+ (manhattan-distance coord to)
                    (apply #'aref lengths-array coord))))
          (let ((loose-ends (make-instance 'pairing-heap:pairing-heap))
                (open (make-hash-table :test #'equal))
                (done (make-hash-table :test #'equal)))
            (setf (apply #'aref lengths-array from) 0)
            (pairing-heap:insert loose-ends (heuristic from) from)
            (loop :for current := (unless (pairing-heap:empty-p loose-ends)
                                    (pairing-heap:extract-min loose-ends))
                  :while current
                  :do (setf (gethash current done) t
                            (gethash current open) nil)
                      (let ((new-length (1+ (apply #'aref
                                                   lengths-array
                                                   current)))
                            (free-neighbours (free-neighbours current)))
                        (dolist (next free-neighbours)
                          (minf (apply #'aref lengths-array next)
                                new-length)
                          (cond ((gethash next done)
                                 :skip)
                                ((equal next to)
                                 (return-from shortest-path-length
                                   new-length))
                                ((gethash next open)
                                 (let ((node (gethash next open))
                                       (new-heuristic (heuristic next)))
                                   (when (< new-heuristic
                                            (pairing-heap::node-key node))
                                     (pairing-heap:decrease-key loose-ends
                                                                node
                                                                new-heuristic))))
                                (t
                                 (setf (gethash next open)
                                       (pairing-heap:insert loose-ends
                                                            (heuristic next)
                                                            next))))))))))))

(defun manhattan-distance (coord-a coord-b)
  (loop :for a :in coord-a
        :and b :in coord-b
        :sum (abs (- a b))))

(defvar *signal-dead-elves-p* nil)

(defun attack (culprit victim)
  (unless (plusp (decf (unit-hit-points victim)
                       (unit-attack-power culprit)))
    (setf (apply #'aref *map* (unit-coord victim)) :free
          *units* (remove victim *units*))
    (when (and *signal-dead-elves-p*
               (char= (unit-type victim) #\E))
      (signal 'elf-died))))

(defun aoc15b (&key (raw-map (load-map)))
  (let ((elf-power 4)
        (largest-fail 3)
        (smallest-win most-positive-fixnum)
        (*signal-dead-elves-p* t))
    (loop (handler-case
              (let* ((*elf-power* elf-power)
                     (result (aoc15a :raw-map raw-map)))
                (setf smallest-win elf-power)
                (when (= (- smallest-win largest-fail) 1)
                  (return (values result elf-power)))
                (setf elf-power (ceiling (+ elf-power largest-fail)
                                         2)))
            (elf-died ()
              (setf largest-fail elf-power
                    elf-power (min (* 2 elf-power)
                                   (ceiling (+ smallest-win elf-power)
                                            2))))))))

(define-condition elf-died () ())

;;; Trying to validate against known battle

(defun execute-walkthrough ()
  (loop :for round :upfrom 0
        :for (wt-round wt-map wt-hit-points)
          :in (parse-walkthrough)
        :for (map units)
          := (multiple-value-list (parse-map wt-map))
            :then (let ((*map* map)
                        (*units* units))
                    (handler-case
                        (progn (setf *units*
                                     (sort *units* #'<
                                           :key (compose (curry #'reading-order
                                                                *map*)
                                                         #'unit-coord)))
                               (execute-round)
                               (list *map* *units*))
                      (end-of-combat ()
                        (let ((remaining-hit-points
                                (reduce #'+
                                        (map 'vector
                                             #'unit-hit-points
                                             *units*))))
                          (print-map *map*)
                          (format t "End of combat.~@
                                     Solution: ~a~@
                                     round: ~a~@
                                     remaining hit points: ~a~%"
                                  (* round remaining-hit-points)
                                  round
                                  remaining-hit-points)
                          (list *map* *units*)))))
        :for map-out := (with-output-to-string (out)
                          (print-map map
                                     :omit-hit-points t
                                     :stream out))
        :for hit-points-out := (format-hit-points units)
        :for map-mismatch := (mismatch map-out wt-map)
        :for hit-points-mismatch := (mismatch hit-points-out wt-hit-points)
        :do (unless (and (equalp map-out wt-map)
                         (equalp hit-points-out wt-hit-points))
              (error "discrepancy found: ~s~%~s~%~%Mismatches: ~a, ~a~%"
                     (list round map-out hit-points-out)
                     (list wt-round wt-map wt-hit-points)
                     map-mismatch
                     hit-points-mismatch))))

(defun format-hit-points (units)
  (let ((raw-output (map 'list
                         (lambda (unit)
                           (list (unit-type unit)
                                 (unit-hit-points unit)))
                         (sort (copy-array units) #'< :key #'unit-id))))
    (assert (= (length raw-output) (length units)))
    (assert (every (curry #'= 2) (mapcar #'length raw-output)))
    (format nil "~{~{~a(~a)~%~}~}" raw-output)))

(defun update-hit-points (units hit-points-array)
  (loop :for unit :across units
        :and (type hit-points) :across hit-points-array
        :do (assert (char= type (unit-type unit))
                    (type unit)
                    "Wrong unit type ~s in hit-points for unit ~s."
                    type unit)
            (setf (unit-hit-points unit) hit-points)))

(defun parse-walkthrough ()
  (with-open-file (in "15-walkthrough")
    (loop :for round := (read-line in nil)
          :for raw-map := (read-lines-starting-with "#" in)
          :for raw-hit-points := (read-lines-starting-with "GE" in)
          :while round
          :collect (list round raw-map raw-hit-points))))

(defun read-lines-starting-with (char-bag stream)
  (with-output-to-string (out)
    (loop :for c := (peek-char t stream nil)
          :while (and c (position c char-bag))
          :do (write-sequence (string-trim '(#\return #\newline)
                                           (read-line stream))
                              out)
              (terpri out))))

(defun parse-hit-points (string)
  (with-input-from-string (in string)
    (coerce (loop :for line := (read-line in nil)
                  :while (plusp (length line))
                  :collect (register-groups-bind (((rcurry #'char 0) type)
                                                  (#'parse-integer hit-points))
                               ("([GE])\\((\\d+)\\)" line)
                             (list type hit-points)))
            'vector)))
