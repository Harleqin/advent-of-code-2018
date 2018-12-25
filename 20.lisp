(in-package #:cl-user)

(defpackage #:aoc-2018-20
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows))

(in-package #:aoc-2018-20)

(defconstant +N+ 0)
(defconstant +E+ 1)
(defconstant +S+ 2)
(defconstant +W+ 3)
(define-constant +north+ #(-1 0) :test #'equalp)
(define-constant +east+ #(0 1) :test #'equalp)
(define-constant +south+ #(1 0) :test #'equalp)
(define-constant +west+ #(0 -1) :test #'equalp)

(defstruct ends
  current
  group-starts
  group-ends
  parent-group)

(defun start-group (ends)
  (make-ends :current (ends-current ends)
             :group-starts (ends-current ends)
             :group-ends ()
             :parent-group ends))

(defun next-alternative (ends)
  (setf (ends-current ends) (ends-group-starts ends)
        (ends-group-ends ends) (append (ends-current ends)
                                       (ends-group-ends ends)))
  ends)

(defun end-group (ends)
  (let ((parent (ends-parent-group ends)))
    (setf (ends-current parent) (-> (ends-current ends)
                                    (append (ends-group-ends ends))
                                    (remove-duplicates :test #'equalp)))
    parent))

(defun read-graph (stream)
  (let* ((graph (make-hash-table :test #'equalp))
         (starting-ends (make-ends :current (list #(0 0))
                                   :group-starts (list #(0 0))
                                   :group-ends ()
                                   :parent-group nil)))
    (loop :for c := (read-char stream nil)
          :for open-ends := starting-ends ; can skip first character anyway (^)
            :then (case c
                    (#\N (advance open-ends +N+ +S+ +north+ graph))
                    (#\E (advance open-ends +E+ +W+ +east+ graph))
                    (#\S (advance open-ends +S+ +N+ +south+ graph))
                    (#\W (advance open-ends +W+ +E+ +west+ graph))
                    (#\( (start-group open-ends))
                    (#\| (next-alternative open-ends))
                    (#\) (end-group open-ends)))
          :while c)
    graph))

(defun advance (ends dir rdir step graph)
  (setf (ends-current ends)
        (mapcar (lambda (coord)
                  (let* ((room (ensure-room coord graph))
                         (next-coord (coord+ coord step))
                         (next-room (ensure-room next-coord graph)))
                    (setf (aref room dir) next-coord
                          (aref next-room rdir) coord)
                    next-coord))
                (ends-current ends)))
  ends)

(defun coord+ (a b)
  (map 'vector #'+ a b))

(defun ensure-room (coord graph)
  (ensure-gethash coord graph (make-array 4 :initial-element nil)))

(defun sssp-longest-path (graph)
  (do* ((path-lengths (make-hash-table :test (hash-table-test graph)))
        (l 0 (1+ l))
        (l-coords (list #(0 0))
                  (->> l-coords
                       (mapcan (curry #'next-coords graph))
                       (remove-if (rcurry #'gethash path-lengths)))))
       ((endp l-coords) (1- l))
    (dolist (coord l-coords)
      (setf (gethash coord path-lengths) l))))

(defun next-coords (graph coord)
  (coerce (remove nil (gethash coord graph)) 'list))

(defun aoc20a ()
  (with-open-file (in "20")
    (sssp-longest-path (read-graph in))))

(defun sssp-paths-longer-than (graph n)
  (do* ((path-lengths (make-hash-table :test (hash-table-test graph)))
        (l 0 (1+ l))
        (l-coords (list #(0 0))
                  (->> l-coords
                       (mapcan (curry #'next-coords graph))
                       (remove-if (rcurry #'gethash path-lengths))))
        (distant-room-count 0
                            (if (>= l n)
                                (+ distant-room-count (length l-coords))
                                distant-room-count)))
       ((endp l-coords) distant-room-count)
    (dolist (coord l-coords)
      (setf (gethash coord path-lengths) l))))

(defun aoc20b ()
  (with-open-file (in "20")
    (sssp-paths-longer-than (read-graph in) 1000)))
