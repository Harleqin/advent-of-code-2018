(ns aoc4
  (:require [clojure.instant :as instant]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-time [s]
  (instant/read-instant-date (string/replace s \space \T)))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-event [s]
  (let [[_ time guard text] (re-matches #"\[(.*)\] (?:Guard #(\d*))?(.*)" s)]
    (merge {:time (parse-time time)}
           (cond (string/includes? text "begin") {:type :begin
                                           :guard (parse-int guard)}
                 (string/includes? text "wake") {:type :wake}
                 (string/includes? text "sleep") {:type :sleep}))))

(defn read-events [filename]
  (->>  (line-seq (io/reader filename))
        (map parse-event)
        (sort-by :time)))

(defn partition-guard-events [events]
  (reduce (fn [[m last-guard] {:keys [time type guard] :as event}]
            [(update m
                     (or guard last-guard)
                     (fn [old]
                       (if old
                         (conj old event)
                         [event])))
             (or guard last-guard)])
          [{} nil]
          events))

(defn sleep-sum [events]
  (->> (partition 2 events)
       (map (fn [[sleep wake]]
              (- (.getMinute (:time wake)) (.getMinute (:time sleep)))))
       (reduce +)))
