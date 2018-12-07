(in-package #:aoc-2018)

(defun read-deps-7 ()
  (with-open-file (in "7")
    (loop :for line := (read-line in nil)
          :while (and line (plusp (length line)))
          :collect (parse-dep line))))

(defun parse-dep (string)
  (register-groups-bind (((lambda (s) (char s 0))
                          a b))
      ("Step (.) must be finished before step (.) can begin." string)
    (list a b)))

(defun aoc7a (&optional (deps (read-deps-7)))
  (let ((steps (extract-steps deps))
        (dep-graph (make-dep-graph deps)))
    (coerce (loop :for remaining-steps := steps :then (remove step remaining-steps)
                  :for remaining-graph := dep-graph
                    :then (remove-from-graph remaining-graph step)
                  :for step := (next-step remaining-steps remaining-graph)
                  :while step
                  :collect step)
            'string)))

(defun extract-steps (deps)
  (-> deps
      flatten
      (remove-duplicates :test #'string=)
      (sort #'char<)))

(defun make-dep-graph (deps)
  (let ((dep-graph (make-hash-table)))
    (loop :for dep :in deps
          :do (push (first dep) (gethash (second dep) dep-graph nil)))
    dep-graph))

(defun next-step (steps dep-graph)
  (find-if (lambda (step)
             (endp (gethash step dep-graph)))
           steps))

(defun remove-from-graph (graph step-or-steps
                          &aux (steps (ensure-list step-or-steps)))
  (copy-hash-table graph
                   :key (lambda (deps)
                          (set-difference deps steps))))

(defun aoc7b (&optional (deps (read-deps-7)))
  (loop :with finishing-times := (make-hash-table)
        :and steps := (extract-steps deps)
        :for graph := (make-dep-graph deps) :then new-graph
        :for tick :upfrom 0
        :for (new-graph todo wip done) := (work tick steps wip done graph finishing-times)
          :then (work tick todo wip done graph finishing-times)
        :while (or todo wip)
        :finally (return tick)))

(defparameter *workers* 5)

(defun work (tick todo wip done graph finishing-times)
  (let* ((freshly-done (remove-if-not (lambda (step)
                                        (= tick (gethash step finishing-times)))
                                      wip))
         (remaining-wip (set-difference wip freshly-done))
         (new-graph (remove-from-graph graph freshly-done))
         (new-wip (next-wip (- *workers* (length remaining-wip))
                            todo
                            new-graph)))
    (dolist (step new-wip)
      (setf (gethash step finishing-times)
            (+ tick (duration step))))
    (list new-graph
          (set-difference todo new-wip)
          (append remaining-wip new-wip)
          (append done freshly-done))))

(defun next-wip (n todo graph)
  (loop :for step :in todo
        :when (endp (gethash step graph))
          :collect step
          :and :count t :into r
        :while (< r n)))

(defparameter *modify-duration* 0)

(defun duration (step)
  (- (char-code step) 4 *modify-duration*))
