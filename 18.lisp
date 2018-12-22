(in-package #:cl-user)

(defpackage #:aoc-2018-18
  (:use #:cl
        #:alexandria
        #:aoc-2018))

(in-package #:aoc-2018-18)

(defun read-landscape-from-file (filename)
  (with-open-file (in filename)
    (read-landscape in)))

(defstruct landscape
  "A two-bit-byte array implementation with an integer as backing data
structure, manipulated bitwise.  This is /slower/ than the implementation based
on general 2D arrays, but I hope to get faster hash-table lookups and a much
lower memory footprint."
  (height 50 :type fixnum)
  (width 50 :type fixnum)
  (contents 0 :type integer))

(defconstant +open+ 0)
(defconstant +woods+ 1)
(defconstant +lumberyard+ 2)

(defun read-landscape (stream)
  (multiple-value-bind (cs height width)
      (loop :with widths := ()
            :for line := (read-line stream nil)
            :while line
            :do (pushnew (length line) widths)
            :append (coerce line 'list) :into cs
            :count t :into height
            :finally (assert (= (length widths) 1))
                     (return (values cs
                                     height
                                     (first widths))))
    (assert (= (length cs) (* height width)))
    (loop :for c :in cs
          :for i :upfrom 0 :by 2
          :for bits := (translate c) :then (dpb (translate c)
                                                (byte 2 i)
                                                bits)
          :finally (return (make-landscape :height height
                                           :width width
                                           :contents bits)))))

(defun translate (char)
  (ecase char
    (#\. +open+)
    (#\| +woods+)
    (#\# +lumberyard+)))

(defun landscape-dimensions (landscape)
  (list (landscape-height landscape)
        (landscape-width landscape)))

(defun landscape-total-size (landscape)
  (* (landscape-height landscape)
     (landscape-width landscape)))

(defun landscape-in-bounds-p (landscape y x)
  (and (< -1 y (landscape-height landscape))
       (< -1 x (landscape-width landscape))))

(defun ls-ref (landscape y x)
  #+debug (assert (landscape-in-bounds-p landscape y x))
  (ldb (ls-byte landscape y x)
       (landscape-contents landscape)))

(defun (setf ls-ref) (new-value landscape y x)
  #+debug (assert (landscape-in-bounds-p landscape y x))
  #+debug (check-type new-value (integer 0 (3)))
  (setf (landscape-contents landscape)
        (dpb new-value
             (ls-byte landscape y x)
             (landscape-contents landscape))))

(defun row-major-ls-ref (landscape i)
  #+debug (assert (< i (landscape-total-size landscape)))
  (ldb (byte 2 (* 2 i)) (landscape-contents landscape)))

(defun ls-byte (landscape y x)
   (byte 2 (* 2 (+ (* y (landscape-width landscape)) x))))

(defun neighbour-counts (landscape y x)
  (frequencies (loop :for ny :from (1- y) :to (1+ y)
                     :append (loop :for nx :from (1- x) :to (1+ x)
                                   :when (and (landscape-in-bounds-p landscape ny nx)
                                              (or (/= ny y)
                                                  (/= nx x)))
                                     :collect (ls-ref landscape ny nx)))))

(defun update (landscape y x)
  (let ((neighbour-counts (neighbour-counts landscape y x)))
    (case (ls-ref landscape y x)
      (0 (if (>= (gethash +woods+ neighbour-counts 0) 3)
                  +woods+
                  +open+))
      (1 (if (>= (gethash +lumberyard+ neighbour-counts 0) 3)
                   +lumberyard+
                   +woods+))
      (2 (if (and (plusp (gethash +lumberyard+ neighbour-counts 0))
                             (plusp (gethash +woods+ neighbour-counts 0)))
                        +lumberyard+
                        +open+)))))

(defun aoc18a (&optional (landscape (read-landscape-from-file "18")))
  (destructuring-bind (height width) (landscape-dimensions landscape)
    (loop :repeat 10
          :for (current next) :on (circular-list landscape
                                                 (copy-landscape landscape))
          :do (dotimes (y height)
                (dotimes (x width)
                  (setf (ls-ref next y x)
                        (update current y x))))
          :finally (return (loop :for i :below (landscape-total-size next)
                                 :for acre := (row-major-ls-ref landscape i)
                                 :count (= acre +woods+) :into wooded
                                 :count (= acre +lumberyard+) :into lumberyards
                                 :finally (return (* wooded lumberyards)))))))

(defun aoc18b (&key
                 (landscape (read-landscape-from-file "18"))
                 (target-round 1000000000))
  (destructuring-bind (height width) (landscape-dimensions landscape)
    (loop :with seen := (make-hash-table)
          :for round :upfrom 0 :below target-round
          :for (current next) :on (circular-list landscape
                                                 (copy-landscape landscape))
          :do (if-let ((seen-round (gethash (landscape-contents current) seen)))
                (let ((cycle-length (- round seen-round)))
                  (incf round (* cycle-length
                                 (floor (- target-round round)
                                        cycle-length)))
                  (clrhash seen))
                (setf (gethash (landscape-contents current) seen) round))
              (dotimes (y height)
                (dotimes (x width)
                  (setf (ls-ref next y x)
                        (update current y x))))
          :finally (return (loop :for i :below (landscape-total-size next)
                                 :for acre := (row-major-ls-ref landscape i)
                                 :count (= acre +woods+) :into wooded
                                 :count (= acre +lumberyard+) :into lumberyards
                                 :finally (return (* wooded lumberyards)))))))
