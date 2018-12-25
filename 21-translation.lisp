(let ((r0 test)
      (r1 0)
      (r3 0)
      (r4 0)
      (r5 0))
  (tagbody
     (setf r4 123)
   1
     (setf r4 (logand 456 r4))
     (when (/= r4 72)
       (go 1))
     (setf r4 0)
   6
     (setf r5 (logior r4 65536))
     (setf r4 1765573)
   8
     (setf r1 (logand r5 255))
     (incf r4 r1)
     (setf r4 (logand r4 16777215))
     (setf r4 (* r4 65899))
     (setf r4 (logand r4 16777215))
     (when (> 256 r5)
       (go 28))
     (setf r1 0)
   18
     (setf r3 (+ r1 1))
     (setf r3 (* r3 256))
     (when (> r3 r5)
       (go 26))
     (incf r1)
     (go 18)
   26
     (setf r5 r1)
     (go 8)
   28
     (when (= r4 r0)
       (go halt))
     (go 6)
   halt))

(let ((r0 test)
      (r1 0)
      (r3 0)
      (r4 0)
      (r5 0))
  (tagbody
   6
     (setf r5 (logior r4 65536))
     (setf r4 1765573)
   8
     (setf r1 (logand r5 255))
     (incf r4 r1)
     (setf r4 (logand r4 16777215))
     (setf r4 (* r4 65899))
     (setf r4 (logand r4 16777215))
     (when (> 256 r5)
       (go 28))
     (setf r5
           (loop :for r1 :from 0
                 :for r3 := (* (1+ r1) 256)
                 :until (> r3 r5)
                 :finally (return r1)))
     (go 8)
   28
     (when (= r4 r0)
       (go halt))
     (go 6)
   halt))

(let ((r0 test)
      (r1 0)
      (r3 0)
      (r4 0)
      (r5 0))
  (flet ((frob (f4 f5)
           (-> f4
               (+ (mod f5 256))
               (mod (expt 2 24))
               (* 65899)
               (mod (expt 2 24)))))
    (tagbody
     6
       (setf r4 1765573)
       (setf r4
         (loop :for l5 :downfrom (logior r4 65536) :downto 256 :by (rcurry #'floor 256)
               :for l4 := (frob r4 l5) :then (frob l4 l5)
               :finally (return (values l4 l5))))
       (when (= r4 r0)
         (go halt))
       (go 6)
     halt)))

(let ((r0 test)
      (r1 0)
      (r3 0)
      (r4 0)
      (r5 0))
  (labels ((+mod (a b)
             (mod (+ a b) (expt 2 24)))
           (*mod (a b)
             (mod (* a b) (expt 2 24)))
           (byte-array (n)
             (loop :with l := (ceiling (integer-length n) 8)
                   :with array := (make-array l
                                              :element-type '(unsigned-byte 8))
                   :for i :upfrom 0
                   :do (setf (aref array i)
                             (ldb (byte 8 (* 8 i)) n))
                   :finally (return array)))
           (frob (f4 byte)
             (-> f4
                 (+mod byte)
                 (*mod 65899)))
           (twiddle (l5)
             (reduce #'frob
                     (byte-array l5)
                     :initial-value 1765573)))
    (tagbody
     6
       (setf r5 (logior r4 65536))
       (setf r4 (twiddle r5))
       (when (= r4 r0)
         (go halt))
       (go 6)
     halt)))

(defun prog (test)
  (labels ((+mod (a b)
             (mod (+ a b) (expt 2 24)))
           (*mod (a b)
             (mod (* a b) (expt 2 24)))
           (byte-array (n)
             (loop :with l := (ceiling (integer-length n) 8)
                   :with array := (make-array l
                                              :element-type '(unsigned-byte 8))
                   :for i :upfrom 0
                   :do (setf (aref array i)
                             (ldb (byte 8 (* 8 i)) n))
                   :finally (return array)))
           (frob (f4 byte)
             (-> f4
                 (+mod byte)
                 (*mod 65899)))
           (twiddle (l5)
             (reduce #'frob
                     (byte-array l5)
                     :initial-value 1765573)))
    (loop :for t5 := 65536 :then (logior t4 65536)
          :for t4 := (twiddle t5)
          :until (= t4 test))))
