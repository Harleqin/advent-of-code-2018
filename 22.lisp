(in-package #:cl-user)

(defpackage #:aoc-2018-22
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2018-22)

(defun read-input ()
  (let ((input (with-open-file (in "22")
                 (register-groups-bind ((#'parse-integer depth))
                     ("depth: (\\d+)" (read-line in))
                   (register-groups-bind ((#'parse-integer x y))
                       ("target: (\\d+),(\\d+)" (read-line in))
                     (list depth (vector y x)))))))
    (check-type input
                (cons integer
                      (cons (vector integer 2))))
    input))

(defenum
  +rocky+
  +wet+
  +narrow+)

(defstruct region
  type
  geologic-index
  erosion-level)

(defparameter *padding* 200
  "This is added to both map dimensions at the beginning and whenever a search
coordinate would be outside the already allocated map.")

(defun create-map (depth target)
  (let* ((height (+ (aref target 0) 1 *padding*))
         (width (+ (aref target 1) 1 *padding*))
         (map (make-array (list height width)
                          :element-type '(or region null)
                          :initial-element nil)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref map y x)
              (create-region map y x depth target))))
    (adjust-array map (array-dimensions map)
                  :element-type 'region)))

(defun create-region (map y x depth target)
  (let* ((geologic-index
           (cond ((= y x 0)
                  0)
                 ((equalp (vector y x) target)
                  0)
                 ((zerop y)
                  (* x 16807))
                 ((zerop x)
                  (* y 48271))
                 (t
                  (* (region-erosion-level (aref map y (1- x)))
                     (region-erosion-level (aref map (1- y) x))))))
         (erosion-level
           (-> geologic-index
               (+ depth)
               (mod 20183)))
         (type (mod erosion-level 3)))  ; note slight bias against +narrow+
    (make-region :type type
                 :geologic-index geologic-index
                 :erosion-level erosion-level)))

(defun print-map (map target &optional (stream t))
  (dotimes (y (array-dimension map 0))
    (dotimes (x (array-dimension map 1))
      (princ (if (equalp (vector y x) target)
                 #\T
                 (char ".=|" (region-type (aref map y x)))) stream))
    (terpri stream)))

(defun risk-level (map target)
  (loop :for y :to (aref target 0)
        :sum (loop :for x :to (aref target 1)
                   :sum (region-type (aref map y x)))))

(defun aoc22a ()
  (destructuring-bind (depth target) (read-input)
    (risk-level (create-map depth target)
                target)))

;;; Part 2

;; The map might need to grow.

(defun expand-map (map depth)
  (let ((new (make-array (mapcar (curry #'+ *padding*)
                                 (array-dimensions map))
                         :element-type '(or region null)
                         :initial-element nil)))
    (dotimes (y (array-dimension map 0))
      (dotimes (x (array-dimension map 1))
        (setf (aref new y x) (aref map y x))))
    (loop :for y :from (array-dimension map 0) :below (array-dimension new 0)
          :do (dotimes (x (array-dimension map 1))
                (setf (aref new y x)
                      (create-region new y x depth #(-1 -1)))))
    (dotimes (y (array-dimension new 0))
      (loop :for x :from (array-dimension map 1) :below (array-dimension new 1)
            :do (setf (aref new y x)
                      (create-region new y x depth #(-1 -1)))))
    (adjust-array new (array-dimensions new)
                  :element-type 'region)))

;; New coordinate definition: equipment as additional coordinate

(defun coord+ (a b)
  (let ((next (map 'vector #'+ a b)))
    (setf (aref next 2)
          (mod (aref next 2) 3))
    next))

(defvar *directions*
  (vector #(-1 0 0)                     ; north
          #(0 1 0)                      ; east
          #(1 0 0)                      ; south
          #(0 -1 0)                     ; west
          #(0 0 1)                      ; equip+
          #(0 0 -1)))                   ; equip-

(defenum
  +neither+
  +torch+
  +climbing-gear+)

;; A wrapper for a heap that keeps track of already present values.

(defstruct unique-heap
  (heap (make-instance 'pairing-heap:pairing-heap))
  (nodes (make-hash-table :test #'equalp)))

(defun heap-upsert (uheap key value)
  (let+ (((&structure unique-heap- heap nodes) uheap))
    (if-let ((node (gethash value nodes)))
      (when (< key (pairing-heap::node-key node))
        (pairing-heap:decrease-key heap node key))
      (setf (gethash value nodes)
            (pairing-heap:insert heap key value)))))

(defun heap-pop (uheap)
  (let+ (((&structure unique-heap- heap nodes) uheap))
    (unless (pairing-heap:empty-p heap)
      (let ((value (pairing-heap:extract-min heap)))
        (remhash value nodes)
        value))))

;; The graph nodes just need the length traveled to them and all valid exits (a
;; subset of *directions*).

(defstruct node
  distance
  exits)

(defvar *map*)
(defvar *depth*)

(defun shortest-path-length (target)
  (let ((graph (make-hash-table :test #'equalp))
        (heap (make-unique-heap))
        (mouth (vector 0 0 +torch+)))
    (setf (gethash mouth graph)
          (make-node :distance 0
                     :exits (find-exits mouth)))
    (heap-upsert heap
                 (heuristic mouth 0 target)
                 mouth)
    (loop :for current-coord := (heap-pop heap)
          :while current-coord
          :do (let ((current-node (gethash current-coord graph)))
                (dovector (next-coord (node-exits current-node))
                  (let ((new-length (+ (node-distance current-node)
                                       (if (= (mismatch current-coord
                                                        next-coord)
                                              2)
                                           7
                                           1))))
                    (when (equalp next-coord target)
                      (return-from shortest-path-length new-length))
                    (unless (some-> (gethash next-coord graph)
                                    node-distance
                                    (<= new-length))
                      (setf (gethash next-coord graph)
                            (make-node :distance new-length
                                       :exits (find-exits next-coord)))
                      (heap-upsert heap
                                   (heuristic next-coord new-length target)
                                   next-coord))))))))

(defun find-exits (coord)
  (->> *directions*
       (map 'vector (curry #'coord+ coord))
       (remove-if-not #'valid-coord)))

(defun valid-coord (coord)
  (let+ ((#(y x e) coord))
    (and (not (minusp y))
         (not (minusp x))
         (fit-equipment-p (ensure-in-bounds-aref-*map* y x) e))))

(defun ensure-in-bounds-aref-*map* (y x)
  (loop :until (array-in-bounds-p *map* y x)
        :do (setf *map* (expand-map *map* *depth*)))
  (aref *map* y x))

(defun fit-equipment-p (region equipment)
  (/= (region-type region) equipment))  ; integer enum punning par excellence

(defun heuristic (coord distance target)
  (let+ ((#(y x e) coord)
         (#(ty tx te) target))
    (+ distance
       (abs (- ty y))
       (abs (- tx x))
       (if (= te e) 0 7))))

(defun aoc22b ()
  (destructuring-bind (*depth* target) (read-input)
    (let ((*map* (create-map *depth* target)))
      (shortest-path-length (concatenate 'vector
                                         target
                                         (vector +torch+))))))
