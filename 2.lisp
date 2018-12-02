(in-package #:aoc-2018)

(defun read-ids (filename)
  (with-open-file (in filename)
    (loop :for line := (read-line in nil)
          :while (and line (plusp (length line)))
          :collect line)))

(defun tupletsp (string n)
  (loop :for c :across (remove-duplicates string :test #'char=)
        :thereis (= (count c string :test #'char=) n)))

(defun aoc2a (&optional (ids (read-ids "2")))
  (loop :for id :in ids
        :count (tupletsp id 2) :into twos
        :count (tupletsp id 3) :into threes
        :finally (return (* twos threes))))

(defun dissimilarity (a b)
  (loop :for ca :across a
        :for cb :across b
        :count (char/= ca cb)))

(defun common-chars (a b)
  (let ((p (mismatch a b :test #'char=)))
    (with-output-to-string (out)
      (write-sequence a out :end p)
      (write-sequence b out :start (1+ p)))))

(defun aoc2b (&optional (ids (read-ids "2")))
  (loop :for (id . rest) :on ids
          :thereis (loop :for other :in rest
                           :thereis (when (= (dissimilarity id other) 1)
                                      (common-chars id other)))))
