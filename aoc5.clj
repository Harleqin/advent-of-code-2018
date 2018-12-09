(ns aoc5
  (:require [clojure.java.io :as io]))

(defn read-polymer [filename]
  (map int
       (first (line-seq (io/reader filename)))))

(defn react [polymer]
  (reduce (fn [to next]
            (if (and (seq to)
                     (= (Math/abs (- (peek to) next)) 32))
              (pop to)
              (conj to next)))
          []
          polymer))

(defn aoc5a
  ([]
   (aoc5a (read-polymer "5")))
  ([polymer]
   (count (react polymer))))

(defn remove-type [polymer type]
  (remove #{type (+ type 32)} polymer))

(defn aoc5b
  ([]
   (aoc5b (read-polymer "5")))
  ([polymer]
   (reduce min
           (map #(aoc5a (remove-type polymer %))
                (range (int \A) (inc (int \Z)))))))
