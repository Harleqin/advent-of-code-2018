(in-package #:cl-user)

(defpackage #:aoc-2018-3
  (:use #:cl
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2018-3)

(defun aoc3a (&optional (claims (read-claims "3")))
  (let ((cloth (marked-cloth claims)))
    (count-if (lambda (e) (> e 1))
              (make-array (* 1000 1000) :displaced-to cloth))))

(defun marked-cloth (claims)
  (let ((cloth (make-array '(1000 1000)
                           :initial-element 0)))
    (loop :for claim :in claims
          :do (mark-claim cloth claim))
    cloth))

(defstruct claim
  id
  top
  left
  height
  width)

(defun read-claims (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while line
          :collect (parse-claim line))))

(defun parse-claim (string)
  (register-groups-bind ((#'parse-integer id left top width height))
      ("#(\\d+)\\s*@\\s*(\\d+),(\\d+):\\s*(\\d+)x(\\d+)" string)
    (make-claim :id id
                :top top
                :left left
                :height height
                :width width)))

(defmacro do-claim-coords ((x y claim) &body body)
  `(loop :for ,y :from (claim-top ,claim)
         :repeat (claim-height ,claim)
         :do (loop :for ,x :from (claim-left ,claim)
                   :repeat (claim-width ,claim)
                   :do ,@body)))

(defun mark-claim (cloth claim)
  (do-claim-coords (x y claim)
    (incf (aref cloth y x)))
  cloth)

(defun aoc3b (&optional (claims (read-claims "3")))
  (let ((cloth (marked-cloth claims)))
    (flet ((no-overlap-p (claim)
             (do-claim-coords (x y claim)
               (when (> (aref cloth y x) 1)
                 (return-from no-overlap-p nil)))
             t))
      (->> claims
           (find-if #'no-overlap-p)
           (claim-id)))))

