(ns aoc4
  (:require [clojure.instant :as instant]
            [clojure.java.io :as io]))

(defn parse-time [s]
  (instant/parse-timestamp (replace s \space \T)))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-event [s]
  (let [[time guard text] (re-matches #"\[(.*)\] (?:Guard #(\d*))?(.*)" s)]
    (merge {:time (parse-time time)}
           (cond (includes? text "begin") {:type :begin
                                           :guard (parse-int guard)}
                 (includes? text "wake") {:type :wake}
                 (includes? text "sleep") {:type :sleep}))))

(defn read-events [filename]
  (->>  (line-seq (io/reader filename))
        (map parse-event)
        (sort-by :time)))
