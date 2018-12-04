(ns aoc3
  (:require [clojure.java.io :as io]))

(defn parse-claim [s]
  (let [[id top left height width]
        (map #(Integer/parseInt %)
             (rest (re-matches #"#(\d+)\s*@\s*(\d+),(\d+):\s*(\d+)x(\d+)"
                               s)))]
    {:id id
     :top top
     :left left
     :height height
     :width width}))

(defn read-claims [filename]
  (map parse-claim
       (line-seq (io/reader filename))))

(defn make-cloth [initial-element]
  (vec (repeat (* 1000 1000) initial-element)))

(defn claim-points [claim]
  (for [y (range (:top claim) (+ (:top claim) (:height claim)))
        x (range (:left claim) (+ (:left claim) (:width claim)))]
    (+ (* 1000 y) x)))

(defn mark-claim [cloth claim]
  (reduce (fn [c p]
            (update c p inc))
          cloth
          (claim-points claim)))

(defn marked-cloth [claims]
  (reduce (fn [cloth claim]
            (mark-claim cloth claim))
          (make-cloth 0)
          claims))

(defn aoc3a
  ([]
   (aoc3a (read-claims "3")))
  ([claims]
   (let [marked-cloth (marked-cloth claims)]
     (->> marked-cloth
          (filter #(> % 1))
          count))))

(defn aoc3b
  ([]
   (aoc3b (read-claims "3")))
  ([claims]
   (let [marked-cloth (marked-cloth claims)]
    (loop [[c & cs] claims]
      (if (every? (fn [p]
                    (= 1 (get marked-cloth p)))
                  (claim-points c))
        (:id c)
        (recur cs))))))
