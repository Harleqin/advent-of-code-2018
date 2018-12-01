(ns aoc1
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader]))

(defn read-integers [filename]
  (with-open [ir (io/reader filename)]
    (let [pbr (PushbackReader. ir)]
      (->> (repeatedly #(edn/read {:eof nil} pbr))
           (take-while identity)
           vec))))

(defn aoc1a
  ([]
   (aoc1a (read-integers "1")))
  ([v]
   (reduce + v)))

(defn aoc1b
  ([]
   (aoc1b (read-integers "1")))
  ([v]
   (loop [[f & fs] (reductions + (cycle v))
          seen #{}]
     (or (seen f)
         (recur fs (conj seen f))))))
