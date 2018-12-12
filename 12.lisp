(in-package #:cl-user)

(defpackage #:aoc-2018-12
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2018-12)

(defstruct state
  offset
  pots)

(defun aoc12a (&optional
                 (initial-state (read-initial-state))
                 (rules (read-rules)))
  (generations-sum 20 initial-state rules))

(defun generations-sum (n initial-state rules)
  (loop :repeat n
        :for state := (next-gen initial-state rules)
          :then (next-gen state rules)
        :finally (return (values (sum-pots state)
                                 state))))

(defun sum-pots (state)
  (loop :for pots := (state-pots state) :then (ash pots -1)
        :for i :upfrom (state-offset state)
        :until (zerop pots)
        :when (logbitp 0 pots)
          :sum i))

(defun read-initial-state ()
  (with-open-file (in "12")
    (parse-state (subseq (read-line in)
                         (length "initial state: ")))))

(defun parse-state (string &optional (add-offset 0))
  (multiple-value-bind (pots offset)
      (truncate-zeroes (parse-pots string))
    (make-state :offset (+ offset add-offset)
                :pots pots)))

(defun render-state (state padding)
  (when (< padding (state-offset state))
    (error "Padding ~a not large enough for state offset ~a."
           padding
           (state-offset state)))
  (let ((bin (ash (state-pots state) (+ padding (state-offset state)))))
    (with-output-to-string (out)
      (loop :for i :below (integer-length bin)
            :do (princ (if (logbitp i bin) #\# #\.) out)))))

(defun parse-pots (string)
  "Parses a string of #\# and #\. into a bitarray represented as an integer.
The first character becomes byte 0 in that integer, i. e. the least significant
bit."
  (loop :for c :across string
        :for bit := (if (char= c #\#) 1 0)
        :for i :upfrom 0
        :for pots := bit :then (+ (ash bit i) pots)
        :finally (return pots)))

(defun truncate-zeroes (bin)
  "Removes all consecutive zeroes from the least significant end of integer BIN.
Returns the truncated number and the count of removed zeroes."
  (if (zerop bin)
      (values 0 0)
      (let ((offset (loop :for bit :upfrom 0
                          :until (logbitp bit bin)
                          :finally (return bit))))
        (values (ash bin (* offset -1))
                offset))))

(defun read-rules ()
  (with-open-file (in "12")
    (loop :repeat 2 :do (read-line in))
    (-> (loop :for line := (read-line in nil)
              :while line
              :collect (parse-rule line))
        (alist-hash-table))))

(defun parse-rule (string)
  (cons (parse-pots (subseq string 0 5))
        (if (char= (char string 9)
                   #\#)
            1
            0)))

(defun next-gen (state rules)
  (loop :for pots := (ash (state-pots state) 4) :then (ash pots -1)
        :for pattern := (ldb (byte 5 0) pots)
        :for i :upfrom 0
        :for next-pots := (gethash pattern rules 0)
          :then (+ (ash (gethash pattern rules 0) i) next-pots)
        :until (zerop pots)
        :finally (return (multiple-value-bind (new-pots offset)
                             (truncate-zeroes next-pots)
                           (make-state :offset (+ (state-offset state)
                                                  -2
                                                  offset)
                                       :pots new-pots)))))

(defun aoc12b (&optional
                 (initial-state (read-initial-state))
                 (rules (read-rules)))
  (generations-sum 50000000 initial-state rules))
