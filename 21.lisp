(in-package #:cl-user)

(defpackage #:aoc-2018-21
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows))

(in-package #:aoc-2018-21)

;;; Translated program.  For translation (decompilation) steps, see
;;; 21-translation.lisp.

(defun +mod (a b)
  (mod (+ a b) (expt 2 24)))

(defun *mod (a b)
  (mod (* a b) (expt 2 24)))

(defun byte-array (n)
  (loop :with l := (ceiling (integer-length n) 8)
        :with array := (make-array l
                                   :element-type '(unsigned-byte 8))
        :for i :below l
        :do (setf (aref array i)
                  (ldb (byte 8 (* 8 i)) n))
        :finally (return array)))

(defun frob (f4 byte)
  (-> f4
      (+mod byte)
      (*mod 65899)))

(defun twiddle (l5)
  (reduce #'frob
          (byte-array l5)
          :initial-value 1765573))

(defun elfprog21 (test)
  (loop :for t5 := 65536 :then (logior t4 65536)
        :for t4 := (twiddle t5)
        :until (= t4 test)))

;;; Part 1

;; As far as I can see, this means the value after first iteration of the loop.
;; Note that this gives the condition “after the fewest instructions” a higher
;; priority than the condition “lowest non-negative integer”.  Note also that
;; the latter doesn't make sense then, while if it were the other way around, it
;; wouldn't make sense at all.
(defun aoc21a ()
  (twiddle 65536))

;;; Part 2

;; Since the tested value is a (mod (expt 2 24)), it will always be
;; non-negative.  For the conditions to hold, we must assume that the loop will
;; enter a cycle somewhere.  The answer is then the last number before
;; repetition.  Note that “lowest non-negative integer” is irrelevant here,
;; again.  However, in this case, the other interpretation (minimize t4 into
;; answer) would have made sense, too, and the only hint I see that it is this
;; way around is the interpretation of the first part.
(defun aoc21b ()
  (loop :with seen := (make-hash-table)
        :for i :from 0
        :for t5 := 65536 :then (logior t4 65536)
        :for last := nil :then t4
        :for t4 := (twiddle t5)
        :do (if-let ((s (gethash t4 seen)))
              (return (values last i s))
              (setf (gethash t4 seen) i))))
