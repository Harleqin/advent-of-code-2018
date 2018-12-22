(in-package #:cl-user)

(defpackage #:aoc-2018-17
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre
        #:let-plus
        #:split-sequence))

(in-package #:aoc-2018-17)

(defun read-clay-from-file ()
  (with-open-file (in "17")
    (read-clay in)))

(defun read-clay (stream)
  (loop :for line := (read-line stream nil)
        :while line
        :append (parse-vein line)))

(defun parse-vein (string)
  "Gives a list of all clay squares in this vein, in (y . x) order."
  (register-groups-bind (((rcurry #'char 0) dim) (#'parse-integer a bmin bmax))
      ("([xy])=(\\d+), [xy]=(\\d+)..(\\d+)" string)
    (mapcar (lambda (b)
              (ecase dim
                (#\x (list b a))
                (#\y (list a b))))
            (iota (1+ (- bmax bmin))
                  :start bmin))))

(defun create-field (clay)
  (destructuring-bind (ymin ymax xmin xmax) (ranges clay)
    (declare (ignore ymin xmin))
    (let ((field (make-array (list (1+ ymax) (+ xmax 2))
                             :initial-element #\.)))
      (setf (aref field 0 500) #\+)
      (loop :for (y x) :in clay
            :do (setf (aref field y x) #\#))
      field)))

(defun ranges (coords)
  (loop :for (y x) :in coords
        :minimize y :into ymin
        :maximize y :into ymax
        :minimize x :into xmin
        :maximize x :into xmax
        :finally (return (list ymin ymax xmin xmax))))

(defun print-field (field &key (min-x 0) max-x (min-y 0) max-y)
  (loop :for y :from min-y :below (or max-y (array-dimension field 0))
        :do (loop :for x :from min-x :below (or max-x (array-dimension field 1))
                  :do (princ (aref field y x)))
            (terpri)))

(defvar *field*)

(defvar *step*) ; debug

(defun aoc17a (&key
                 (clay (read-clay-from-file))
                 (*field* (create-field clay)))
  (loop :for previous-open := nil :then open
        :for open := '((1 500))
          :then (->> (mapcan #'advance open)
                     (->* (remove-duplicates :test #'equalp))
                     (remove-if #'been-there))
        :for *step* :below 100          ; debug
        :for debug-vis := (visualize (format nil "~3,,,'0@a" *step*)
                                     open)
        :while open
        :finally (return (count-water *field*))))

(defun count-water (field)
  (let* ((width (array-dimension field 1))
         (ymin (loop :for y :upfrom 0
                     :while (loop :for x :below width
                                  :never (char= (aref field y x) #\#))
                     :count t))
         (offset (* ymin width))
         (flat-view (make-array (- (array-total-size field) offset)
                                :displaced-to field
                                :displaced-index-offset offset)))
    (values (count-if #'waterp flat-view)
            (count #\~ flat-view))))

(defun waterp (char)
  (find char "|~"))

(defun been-there (coord)
  (waterp (apply #'aref *field* coord)))

(defun advance (coord)
  (when-let ((splat-y (flow-down coord)))
    (loop :with fill-x := (second coord)
          :for fill-y :downfrom splat-y :above 0
          :for (left-x left-type) := (fill-left fill-y fill-x)
          :for (right-x right-type) := (fill-right fill-y fill-x)
          :while (char= left-type right-type #\#)
          :do (loop :for freeze-x :from left-x :to right-x
                    :do (setf (aref *field* fill-y freeze-x) #\~))
          :finally (return (cond->> ()
                                    ((char= left-type #\+)
                                     (cons (list fill-y left-x)))
                                    ((char= right-type #\+)
                                     (cons (list fill-y right-x))))))))

(defun flow-down (coord)
  (destructuring-bind (from-y x) coord
    (loop :for y :upfrom from-y :below (array-dimension *field* 0)
          :when (find (aref *field* y x) "#~")
            :do (return (1- y))
          :do (setf (aref *field* y x) #\|))))

(defun fill-left (fill-y fill-x)
  (loop :for x :downfrom fill-x
        :when (char= (aref *field* fill-y x) #\#)
          :do (return (list (1+ x) #\#))
        :when (find (aref *field* (1+ fill-y) x) ".|")
          :do (return (list x #\+))
        :do (setf (aref *field* fill-y x) #\|)))

(defun fill-right (fill-y fill-x)
  (loop :for x :upfrom fill-x
        :when (char= (aref *field* fill-y x) #\#)
          :do (return (list (1- x) #\#))
        :when (find (aref *field* (1+ fill-y) x) ".|")
          :do (return (list x #\+))
        :do (setf (aref *field* fill-y x) #\|)))

(defun visualize (name open)
  (let+ (((height width) (array-dimensions *field*))
         (left-x (block left-x
                   (dotimes (x width)
                     (dotimes (y height)
                       (when (char/= (aref *field* y x) #\.)
                         (return-from left-x (1- x)))))))
         ((h w) (list (+ height 2) (- width left-x -2))) ; 2 borders each
         (y-offset 1)
         (x-offset (- 1 left-x))
         (image (opticl:make-8-bit-rgb-image h w
                                             :initial-contents (checkers h w))))
    (dotimes (y height)
      (loop :for x :from left-x :below width
            :do (setf (opticl:pixel image (+ y y-offset) (+ x x-offset))
                      (case (aref *field* y x)
                        (#\. (values 250 200 150))
                        (#\# (values 150 100 50))
                        (#\| (values 150 150 250))
                        (#\~ (values 200 200 250))
                        (#\+ (values 0 0 250))))))
    (loop :for (y x) :in open
          :do (setf (opticl:pixel image (+ y y-offset) (+ x x-offset))
                    (values 250 100 100)))
    (opticl:write-png-file (concatenate 'string
                                        "p15-"
                                        name
                                        ".png")
                           image)))

(defun checkers (h w)
  (let ((colors (circular-list '(0 0 0) '(255 255 255))))
    (loop :repeat h
          :for row-colors :on colors
          :collect (loop :repeat w
                         :for color :in row-colors
                         :collect color))))

(defun aoc17b ()
  (nth-value 1 (aoc17a)))
