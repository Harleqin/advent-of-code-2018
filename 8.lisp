(in-package #:cl-user)

(defpackage #:aoc-2018-8
  (:use #:cl
        #:aoc-2018
        #:arrows))

(in-package #:aoc-2018-8)

(defstruct node
  children
  metadata)

(defun parse-license (integers)
  (labels ((parse-node ()
             (let ((child-count (pop integers))
                   (meta-count (pop integers)))
               (make-node :children (coerce (loop :repeat child-count
                                                  :collect (parse-node))
                                            'vector)
                          :metadata (loop :repeat meta-count
                                          :collect (pop integers))))))
    (parse-node)))

(defun aoc8a (&optional (integers (read-integers "8")))
  (let ((tree (parse-license integers)))
    (reduce-tree (lambda (acc node)
                   (apply #'+ acc (node-metadata node)))
                 0
                 tree)))

(defun reduce-tree (f init tree)
  (let ((next (funcall f init tree)))
    (if (zerop (length (node-children tree)))
        next
        (loop :for child :across (node-children tree)
              :for acc := (reduce-tree f next child)
                :then (reduce-tree f acc child)
              :finally (return acc)))))

(defun aoc8b (&optional (integers (read-integers "8")))
  (node-value (parse-license integers)))

(defun node-value (node)
  (if (zerop (length (node-children node)))
      (reduce #'+ (node-metadata node))
      (->> (node-metadata node)
           (mapcar (lambda (index)
                     (when (array-in-bounds-p (node-children node) (1- index))
                       (node-value (aref (node-children node) (1- index))))))
           (remove nil)
           (reduce #'+))))
