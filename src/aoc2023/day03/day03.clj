(ns aoc2023.day03.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn digit?
  [ch]
  (boolean (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} ch)))

(defn surrounds
  [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y] [(inc x) y]
   [(dec x) (inc y)] [x (inc y)][(inc x) (inc y)] ])


; test file

(-> (slurp (io/resource "day03/test.txt"))
    (s/replace "\n" "")
    (s/replace "\r" "")
    #_(->> (map-indexed (fn [i e] [i e]))
         (partition-by (comp digit? second))))

(defn coords
  [width]
  (for [x (range)
        y (range width)]
    [x y]))

(defn read-file
  [fname]
  (-> (slurp (io/resource fname))
      (s/replace "\n" "")
      (s/replace "\r" "")))

(defn reduce-thing
  [thing]
  {:coords (mapv first thing)
   :num (map second thing)
   :type (cond (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} (second (first thing))) :number
               :else :symbol)})

(->> (read-file "day03/test.txt")
     (mapv vector (coords 10))
     ;(partition-by #(= \. (second %)))
     ;(remove #(= (second (first %)) \.))
     ;(map reduce-thing)
     )
