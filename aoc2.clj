(ns aoc2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-ids [filename]
  (line-seq (io/reader filename)))

(defn tuplets? [s n]
  (contains? (set (vals (frequencies s))) n))

(defn aoc2a
  ([]
   (aoc2a (read-ids "2")))
  ([ids]
   (* (count (filter #(tuplets? % 2) ids))
      (count (filter #(tuplets? % 3) ids)))))

(defn dissimilarity [a b]
  (->> (map not= a b)
       (filter identity)
       count))

(defn common-chars [a b]
  (->> (map #(when (= %1 %2) %1) a b)
       (remove nil?)
       (apply str)))

(defn aoc2b
  ([]
   (aoc2b (read-ids "2")))
  ([ids]
   (loop [[x & xs] ids]
     (or (->> xs
              (map #(when (= (dissimilarity x %) 1)
                      (common-chars x %)))
              (remove nil?)
              first)
         (recur xs)))))
