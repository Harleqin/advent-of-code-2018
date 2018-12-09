(in-package #:cl-user)

(defpackage #:aoc-2018-9
  (:use  #:cl
         #:aoc-2018
         #:arrows
         #:cl-ppcre))

(in-package #:aoc-2018-9)

(defun parse-input ()
  (with-open-file (in "9")
    (register-groups-bind ((#'parse-integer players last-marble))
        ("(\\d+) players; last marble is worth (\\d+) points" (read-line in))
      (list players last-marble))))

(defun aoc9a (players-count last-marble)
  (loop :with scores := (make-array players-count :initial-element 0)
        :and circle := (make-circle)
        :for marble :to last-marble
        :for player := (mod (1- marble) players-count)
        :do (if (zerop (mod marble 23))
                (multiple-value-bind (new-circle bonus)
                    (circle-remove circle -7)
                  (incf (aref scores player) (+ marble bonus))
                  (setf circle new-circle))
                (setf circle (circle-insert circle 1 marble)))
        :finally (return (reduce #'max scores))))

(defstruct node
  content
  left
  right)

(defun make-circle ()
  (let ((node (make-node :content 0)))
    (setf (node-left node) node
          (node-right node) node)
    node))

(defun circle-insert (circle rel marble)
  "Destructively inserts a new node with MARBLE into CIRCLE after the node REL
to the right of the current node.  Returns the new node, intended as the new
circle reference."
  (let* ((new (make-node :content marble))
         (new-left (circle-rel circle rel))
         (new-right (circle-rel new-left 1)))
    (setf (node-right new-left) new
          (node-left new-right) new
          (node-right new) new-right
          (node-left new) new-left)
    new))

(defun circle-rel (circle rel)
  (case (signum rel)
    (-1 (circle-rel (node-left circle) (1+ rel)))
    (0 circle)
    (+1 (circle-rel (node-right circle) (1- rel)))))

(defun circle-remove (circle rel)
  "Destructively removes the node REL places to the left of the current node of
CIRCLE.  Returns two values: the node to the right of the removed one, and the
content of the removed node."
  (let* ((victim (circle-rel circle rel))
         (victim-left (circle-rel victim -1))
         (victim-right (circle-rel victim 1)))
    (setf (node-left victim-right) victim-left
          (node-right victim-left) victim-right)
    (values victim-right
            (node-content victim))))

(defun aoc9b (players-count last-marble/100)
  (aoc9a players-count (* last-marble/100 100)))
