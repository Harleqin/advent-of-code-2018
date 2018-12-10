(in-package #:cl-user)

(defpackage #:aoc-2018-10
  (:use #:cl
        #:cl-ppcre))

(in-package #:aoc-2018-10)

(defun read-points (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while (plusp (length line))
          :collect (parse-point line))))

(defstruct point
  x
  y
  vx
  vy)

(defun parse-point (string)
  (register-groups-bind ((#'parse-integer x y vx vy))
      ("position=<(.*),(.*)> velocity=<(.*),(.*)>" string)
    (make-point :x x :y y :vx vx :vy vy)))

(defun step-point (point)
  (make-point :x (+ (point-x point) (point-vx point))
              :y (+ (point-y point) (point-vy point))
              :vx (point-vx point)
              :vy (point-vy point)))

(defun make-scene (points)
  (multiple-value-bind (area xmin xmax ymin ymax)
      (area points)
    (declare (ignore area))
    (let ((scene (make-array (list (1+ (- xmax xmin))
                                   (1+ (- ymax ymin)))
                             :element-type 'character
                             :initial-element #\.)))
      (dolist (point points scene)
        (setf (aref scene
                    (- (point-x point) xmin)
                    (- (point-y point) ymin))
              #\#)))))

(defun print-scene (scene)
  (dotimes (y (array-dimension scene 1))
    (dotimes (x (array-dimension scene 0))
      (princ (aref scene x y)))
    (terpri)))

(defun area (points)
  (loop :for point :in points
        :minimize (point-x point) :into xmin
        :maximize (point-x point) :into xmax
        :minimize (point-y point) :into ymin
        :maximize (point-y point) :into ymax
        :finally (return (values (* (- xmax xmin)
                                    (- ymax ymin))
                                 xmin
                                 xmax
                                 ymin
                                 ymax))))

(defun aoc10a (&optional (points (read-points "10"))
               &aux (points-count (length points)))
  (loop :for i :upfrom 0
        :for ps := points :then (mapcar #'step-point ps)
        :for area := (area ps)
        :when (< area (* 3 points-count))
          :do (princ i)
              (terpri)
              (print-scene (make-scene ps))
              (terpri)
              (break)))
