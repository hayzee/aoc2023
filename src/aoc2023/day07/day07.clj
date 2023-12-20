(ns aoc2023.day07.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; part 1

;(def fname "day07/test.txt")
(def fname "day07/part1.txt")

(defn read-file
  [fname]
 (->> (slurp (io/resource fname))
      s/split-lines
      (map (fn [line]
             (let [[hand bid] (s/split line #" ")]
               {:hand hand :bid bid})))))

(def file-data (read-file fname))

(defn hand-type
  [hand-str]
  (let [freqs (frequencies hand-str)
        freqset (set (vals freqs))
        freqfreqs (frequencies (vals freqs))]
    (cond
      (= freqset #{5}) {:type :five-of-a-kind
                        :weight 7}
      (contains? freqset 4) {:type :four-of-a-kind
                             :weight 6}
      (= freqset #{2 3}) {:type :full-house
                          :weight 5}
      (contains? freqset 3) {:type :three-of-a-kind
                             :weight 4}
      (= 2 (get freqfreqs 2)) {:type :two-pair
                               :weight 3}
      (contains? freqset 2) {:type :one-pair
                             :weight 2}
      :else {:type :high-card
             :weight 1})))

(comment

 (hand-type "12345")
 (hand-type "12545")
 (hand-type "42545")
 (hand-type "11213")
 (hand-type "11212")
 (hand-type "11112")
 (hand-type "11111")

 )

(defn augment-hand-type
  [file-data]
  (map (fn [entry]
         (-> (merge entry (hand-type (:hand entry)))
             (update :bid #(Integer/parseInt %)))) file-data))

(def aug-hand (augment-hand-type file-data))

(def card-val
  {\2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn augment-hand-value
  [aug-hand]
  (mapv #(assoc % :hand-value (mapv card-val (:hand %))) aug-hand))

(def aug-hand-vals (augment-hand-value aug-hand))

(def ordered-hands
  (->> aug-hand-vals
       (sort-by #(vec (cons (:weight %) (:hand-value %))) compare)
       (map-indexed (fn [i m] (assoc m :rank (inc i))))
       (map (fn [hand] (assoc hand :winnings (* (:bid hand) (:rank hand)))))
       (map :winnings)
       (reduce +)))

