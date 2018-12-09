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
  (-> (reduce (fn [[m last-guard] {:keys [time type guard] :as event}]
                (if (= type :begin)
                  [m guard]
                  (let [time (.getMinutes time)]
                    [(update m
                             last-guard
                             (fn [old]
                               (if old
                                 (conj old time)
                                 [time])))
                     last-guard])))
              [{} nil]
              events)
      first))

(defn sleep-sum [times]
  (reduce +
          (map #(apply - (reverse %)) times)))

(defn register-minutes [times]
  (reduce (fn [minutes [sleep wake]]
            (reduce (fn [mins i]
                      (update mins i inc))
                    minutes
                    (range sleep wake)))
          (vec (repeat 60 0))
          (partition 2 times)))

(defn aoc4a
  ([]
   (aoc4a (read-events "4")))
  ([events]
   (let [guard-times (partition-guard-events events)
         best-guard (apply max-key
                           (fn [guard]
                             (sleep-sum (partition 2 (guard-times guard))))
                           (keys guard-times))
         minutes (register-minutes (guard-times best-guard))
         best-minute (apply max-key
                            #(get minutes %)
                            (range 0 60))]
     (* best-guard best-minute))))

(defn fmap [f m]
  (reduce (fn [r [k v]]
            (assoc r k (f v)))
          {}
          m))

(defn aoc4b
  ([]
   (aoc4b (read-events "4")))
  ([events]
   (let [guard-times (partition-guard-events events)

         guard-minutes (fmap register-minutes guard-times)

         [best-guard best-minute best-intensity]
         (reduce (fn [[best-guard best-minute best-intensity] [guard minutes]]
                   (let [best-minute-here (apply max-key
                                                 #(get minutes %)
                                                 (range 0 60))
                         best-intensity-here (get minutes best-minute-here)]
                     (if (> best-intensity-here best-intensity)
                       [guard best-minute-here best-intensity-here]
                       [best-guard best-minute best-intensity])))
                 [-1 -1 0]
                 guard-minutes)]
     (* best-guard best-minute))))
