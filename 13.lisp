(in-package #:cl-user)

(defpackage #:aoc-2018-13
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows))

(in-package #:aoc-2018-13)

(defstruct cart
  (round 0)
  (direction #\# :type character)
  (next-turn (circular-list :left :straight :right)))

(defun read-tracks ()
  (parse-tracks (read-file-into-string "13")))

(defun parse-tracks (string)
  (with-input-from-string (in string)
    (let* ((lines (loop :for line := (read-line in nil)
                        :while line
                        :collect line))
           (width (reduce #'max (mapcar #'length lines)))
           (height (length lines))
           (tracks (make-array (list height width)
                               :element-type 'character))
           (carts (make-array (list height width)
                              :initial-element nil
                              :element-type '(or null cart))))
      (loop :for line :in lines
            :for y :upfrom 0
            :do (loop :for c :across line
                      :for x :upfrom 0
                      :do (setf (aref tracks y x)
                                (case c
                                  ((#\< #\>) #\-)
                                  ((#\^ #\v) #\|)
                                  (t c))
                                (aref carts y x)
                                (case c
                                  ((#\< #\> #\^ #\v) (make-cart :direction c))
                                  (t nil)))))
      (values tracks carts))))

(defun with-turn (cart track)
  (case track
    (#\+ (turn cart (pop (cart-next-turn cart))))
    (#\\ (turn cart
               (ecase (cart-direction cart)
                 ((#\< #\>) :right)
                 ((#\^ #\v) :left))))
    (#\/ (turn cart
               (ecase (cart-direction cart)
                 ((#\< #\>) :left)
                 ((#\^ #\v) :right))))
    (t cart)))

(define-constant +directions+ "<^>v"
  :test #'equal)

(defun turn (cart how)
  (unless (eq how :straight)
    (setf (cart-direction cart)
          (char +directions+
                (mod (+ (position (cart-direction cart) +directions+)
                        (ecase how
                          (:left -1)
                          (:right 1)))
                     4))))
  cart)

(defun move (y x cart tracks)
  (multiple-value-bind (yn xn)
      (ecase (cart-direction cart)
        (#\< (values y (1- x)))
        (#\^ (values (1- y) x))
        (#\> (values y (1+ x)))
        (#\v (values (1+ y) x)))
    (let ((cartn (with-turn cart (aref tracks yn xn))))
      (incf (cart-round cart))
      (values yn xn cartn))))

(defun advance (carts tracks round)
  (dotimes (y (array-dimension tracks 0))
    (dotimes (x (array-dimension tracks 1))
      (when-let ((cart (aref carts y x)))
        (unless (> (cart-round cart) round)
          (multiple-value-bind (yn xn cartn)
              (move y x cart tracks)
            (if (aref carts yn xn)
                (return-from advance (list yn xn))
                (setf (aref carts yn xn) cartn
                      (aref carts y x) nil))))))))

(defun aoc13a ()
  (apply #'first-crash (multiple-value-list (read-tracks))))

(defun first-crash (tracks carts)
  (loop :for round :upfrom 0
        :for coord := (advance carts tracks round)
        :thereis (reverse coord)))

(defun display (carts tracks)
  (dotimes (y (array-dimension tracks 0))
    (dotimes (x (array-dimension tracks 1))
      (princ (if-let ((cart (aref carts y x)))
               (cart-direction cart)
               (aref tracks y x))))
    (terpri)))

(defun aoc13b ()
  (apply #'last-cart-standing (multiple-value-list (read-tracks))))

(defun last-cart-standing (tracks carts)
  (loop :for round :upfrom 0
        :for count := (count-if #'identity
                                (make-array (apply #'* (array-dimensions carts))
                                            :displaced-to carts))
          :then (- count (advance-with-crashes carts tracks round))
        :when (evenp count)
          :do (error "Count is even, no cart will survive: ~a." count)
        :until (= count 1)
        :finally (dotimes (y (array-dimension carts 0))
                   (dotimes (x (array-dimension carts 1))
                     (when (aref carts y x)
                       (return-from last-cart-standing (list x y)))))))

(defun advance-with-crashes (carts tracks round)
  (let ((casualties 0))
    (dotimes (y (array-dimension tracks 0))
      (dotimes (x (array-dimension tracks 1))
        (when-let ((cart (aref carts y x)))
          (unless (> (cart-round cart) round)
            (multiple-value-bind (yn xn cartn)
                (move y x cart tracks)
              (if (aref carts yn xn)
                  (progn (setf (aref carts yn xn) nil
                               (aref carts y x) nil)
                         (incf casualties 2))
                  (setf (aref carts yn xn) cartn
                        (aref carts y x) nil)))))))
    casualties))
