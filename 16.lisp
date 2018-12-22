(in-package #:cl-user)

(defpackage #:aoc-2018-16
  (:use #:cl
        #:alexandria
        #:aoc-2018
        #:arrows
        #:cl-ppcre
        #:split-sequence))

(in-package #:aoc-2018-16)

(defun read-inputs ()
  (with-open-file (in "16")
    (values (read-samples in)
            (read-test-program in))))

(defun read-samples (stream)
  (loop :while (char= (peek-char t stream nil) #\B)
        :collect (read-sample stream)))

(defstruct sample
  before
  instruction
  after)

(defun read-sample (stream)
  (make-sample :before (parse-registers (read-line stream))
               :instruction (parse-instruction (read-line stream))
               :after (parse-registers (read-line stream))))

(defun parse-registers (string)
  (register-groups-bind ((#'parse-integer r0 r1 r2 r3))
      ("\\[(\\d+), (\\d+), (\\d+), (\\d+)\\]" string)
    (vector r0 r1 r2 r3)))

(defun parse-instruction (string)
  (map 'vector
       #'parse-integer
       (split-sequence #\space string)))

(defun read-test-program (stream)
  (loop :for line := (read-line stream nil)
        :while line
        :collect (parse-instruction line)))

(defun update (registers index value)
  (let ((r (copy-array registers)))
    (setf (aref r index) value)
    r))

(defvar *ops* (make-hash-table))

(defmacro defop (name (var-registers var-a var-b) &body body)
  (with-gensyms (var-c)
    (multiple-value-bind (forms decl doc) (parse-body body)
      (declare (ignore doc))
      `(setf (gethash ',name *ops*)
             (lambda (,var-registers ,var-a ,var-b ,var-c)
               ,@decl
               (update ,var-registers ,var-c
                       (progn ,@forms)))))))

(defun exec-instruction (registers opname a b c)
  (funcall (gethash opname *ops*) registers a b c))

(defop addr (registers a b)
  (+ (aref registers a) (aref registers b)))

(defop addi (registers a b)
  (+ (aref registers a) b))

(defop mulr (registers a b)
  (* (aref registers a) (aref registers b)))

(defop muli (registers a b)
  (* (aref registers a) b))

(defop banr (registers a b)
  (logand (aref registers a) (aref registers b)))

(defop bani (registers a b)
  (logand (aref registers a) b))

(defop borr (registers a b)
  (logior (aref registers a) (aref registers b)))

(defop bori (registers a b)
  (logior (aref registers a) b))

(defop setr (registers a b)
  (declare (ignore b))
  (aref registers a))

(defop seti (registers a b)
  (declare (ignore b))
  a)

(defop gtir (registers a b)
  (if (> a (aref registers b)) 1 0))

(defop gtri (registers a b)
  (if (> (aref registers a) b) 1 0))

(defop gtrr (registers a b)
  (if (> (aref registers a) (aref registers b)) 1 0))

(defop eqir (registers a b)
  (if (= a (aref registers b)) 1 0))

(defop eqri (registers a b)
  (if (= (aref registers a) b) 1 0))

(defop eqrr (registers a b)
  (if (= (aref registers a) (aref registers b)) 1 0))

(defun aoc16a (&optional (samples (read-inputs)))
  (->> samples
       (mapcar #'possible-ops)
       (mapcar #'length)
       (count-if (rcurry #'>= 3))))

(defun possible-ops (sample)
  (remove-if-not (lambda (opname)
                   (equalp (apply #'exec-instruction
                                  (sample-before sample)
                                  opname
                                  (coerce (subseq (sample-instruction sample) 1)
                                          'list))
                           (sample-after sample)))
                 (hash-table-keys *ops*)))

(defun aoc16b (&optional samples test-program)
  (cond ((null samples)
         (multiple-value-setq (samples test-program)
           (read-inputs)))
        ((null test-program)
         (setf test-program (nth-value 1 (read-inputs)))))
  (let ((opcodes (make-array 16 :initial-element (hash-table-keys *ops*))))
    (loop :for sample :in samples
          :for opcode := (aref (sample-instruction sample) 0)
          :do (setf (aref opcodes opcode)
                    (intersection (aref opcodes opcode)
                                  (possible-ops sample))))
    (loop :with done := (make-array 16 :initial-element nil)
          :until (every #'atom opcodes)
          :do (loop :for donep :across done
                    :for opcode :below 16
                    :for possible := (aref opcodes opcode)
                    :when (and (not donep)
                               (= (length possible) 1))
                      :do (loop :for other-opcode :below 16
                                :when (and (/= opcode other-opcode)
                                           (consp (aref opcodes other-opcode)))
                                  :do (removef (aref opcodes other-opcode)
                                               (first (aref opcodes opcode))))
                          (setf (aref done opcode) t
                                (aref opcodes opcode) (first (aref opcodes opcode)))))
    (loop :for instruction :in test-program
          :for registers := (apply #'exec-instruction
                                   #(0 0 0 0)
                                   (aref opcodes (aref instruction 0))
                                   (coerce (subseq instruction 1) 'list))
            :then (apply #'exec-instruction
                         registers
                         (aref opcodes (aref instruction 0))
                         (coerce (subseq instruction 1) 'list))
          :finally (return (aref registers 0)))))
