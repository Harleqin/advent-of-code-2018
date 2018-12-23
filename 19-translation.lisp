(let ((r0 1)
      (r1 0)
      (r2 0)
      (r3 0)
      (r4 0))
  ;; r5 is instruction pointer only, thus translated to go instructions
  ;; directly.
  (tagbody
     (go 17)                            ; -> addi 3 2 3
   1
     (setf r1 1)
   2
     (setf r2 1)
   3
     (setf r4 (* r1 r2))
     (when (= r4 r3)
       (incf r0 r1))
     (incf r2 1)
     (when (<= r2 r3)
       (go 3))
     (incf r1)
     (when (<= r1 r3)
       (go 2))
     (go halt)
   17
     (incf r3 2)
     (setf r3 (* r3 r3))
     (setf r3 (* r3 19))                ; actually instruction pointer
     (setf r3 (* r3 11))
     (incf r4 8)
     (setf r4 (* r4 22))                ; actually instruction pointer
     (incf r4 13)
     (incf r3 r4)
     #+part1 (go 1)
     (setf r4 27)                       ; actually instruction pointer
     (setf r4 (* r4 28))                ; actually instruction pointer
     (incf r4 29)                       ; actually instruction pointer
     (setf r4 (* r4 30))                ; actually instruction pointer
     (setf r4 (* r4 14))
     (setf r4 (* r4 32))                ; actually instruction pointer
     (incf r3 r4)
     (setf r0 0)
     (go 1)
   halt))

(let ((r0 0)
      (r1 0)
      (r2 0)
      (r3 (+ (* 2 2 19 11)
             (* 8 22)
             13
             (* (+ (* 27 28) 29)
                30
                14
                32))))
  (tagbody
     (setf r1 1)
   2
     (setf r2 1)
   3
     (when (= (* r1 r2) r3)
       (incf r0 r1))
     (incf r2 1)
     (when (<= r2 r3)
       (go 3))
     (incf r1)
     (when (<= r1 r3)
       (go 2))))

(let ((r3 (+ (* 2 2 19 11)
             (* 8 22)
             13
             (* (+ (* 27 28) 29)
                30
                14
                32))))
  (loop :for r1 :from 1 :to r3
        :sum (loop :for r2 :from 1 :to r3
                   :when (= (* r1 r2) r3)
                     :sum (+ r1 r2))))

(let ((r3 (+ (* 2 2 19 11)
             (* 8 22)
             13
             (* (+ (* 27 28) 29)
                30
                14
                32))))
  (loop :for r1 :from 1 :to r3
        :sum (loop :for r2 :downfrom r3 :to r1
                   :while (>= (* r1 r2) r3)
                   :when (= (* r1 r2) r3)
                     :sum (+ r1 r2))))


(let* ((r3 10551425)
       (left 1)
       (right r3)
       (sum 0))
  (loop (when (> left right)
          (return sum))
        (let ((p (* left right)))
          (cond ((= p r3)
                 (incf sum (+ left right))
                 (incf left)
                 (decf right))
                ((< p r3)
                 (incf left))
                ((> p r3)
                 (decf right))))))
