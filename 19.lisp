(in-package #:cl-user)

(defpackage #:aoc-2018-19
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre)
  (:import-from #:aoc-2018-16
                #:exec-instruction
                #:addr
                #:addi
                #:mulr
                #:muli
                #:banr
                #:bani
                #:borr
                #:bori
                #:setr
                #:seti
                #:gtir
                #:gtri
                #:gtrr
                #:eqir
                #:eqri
                #:eqrr))

(in-package #:aoc-2018-19)

(defun read-program-from-file (filename)
  (with-open-file (in filename)
    (read-program in)))

(defun read-program (stream)
  (let ((ip-reg (parse-integer (read-line stream)
                               :start 4))
        (program (coerce (loop :for line := (read-line stream nil)
                               :while line
                               :collect (parse-instruction line))
                         'vector)))
    (assert (not (find nil program)))
    (values program ip-reg)))

(defun parse-instruction (string)
  (register-groups-bind (((compose #'intern #'string-upcase) op)
                         (#'parse-integer a b c))
      ("([a-z]{4})\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)" string)
    (list op a b c)))

(defun aoc19a (&key
                 program
                 ip-reg
                 (initial-registers (make-array 6 :initial-element 0)))
  (assert (eq (null program) (null ip-reg)))
  (when (null program)
    (multiple-value-setq (program ip-reg) (read-program-from-file "19")))
  (let ((registers initial-registers)
        (ip 0))
    (handler-case
        (loop (setf (aref registers ip-reg) ip)
              (print (list :ip ip :registers registers :instruction (aref program ip)))
              (setf registers
                    (apply #'exec-instruction
                           registers
                           (aref program ip)))
              (print (list :registers registers))
              (terpri)
              (break)
              (setf ip (aref registers ip-reg))
              (incf ip))
      (type-error ()
        (aref registers 0)))))

(defun aoc19b ()
  "This is the naïve execution.  The translation steps for a fast result are in
19-translation.lisp."
  (aoc19a :initial-registers (make-array 6 :initial-contents '(1 0 0 0 0 0))))
