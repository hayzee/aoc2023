(ns aoc2023.day04.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; part 1

(defn str->nums
  [str]
  (-> str
      s/trim
      (s/split #"\s+")
      (->> (map #(Integer/parseInt %)))))

;(str->nums " 31 18 13 56 72 ")
;
;(str->nums " 74 77 10 23 35 67 36 11")

(defn read-file
  [fname]
  (-> (slurp (io/resource fname))
      (s/split-lines)
      (->> (map (fn [line] (-> line
                               (s/split #":")
                               second
                               (s/split #"\|"))))
           (map (fn [[ns1 ns2]] {:winners (str->nums ns1) :have (str->nums ns2)})))))


(defn solve
  [fname]
  (->> (read-file fname)
       (map (fn [d] (clojure.set/intersection (set (:winners d)) (set (:have d)))))
       (map count)
       (map #(Math/pow 2 (dec %)))
       (map int)
       (reduce +)))

(solve "day04/test.txt")

(solve "day04/part1.txt")

; part 2

(defn solve2
  [fname]
  (->> (read-file fname)
       (map (fn [d] (clojure.set/intersection (set (:winners d)) (set (:have d)))))
       (map count)
       ;(map #(Math/pow 2 (dec %)))
       ;(map int)
       ;(reduce +)
       ))

(solve2 "day04/test.txt")


(4 2 2 1 0 0)

(defn expand
  [s]
  (println s)
  (when s
    (concat (cons (first s) (expand (take (first s) (rest s)))))))

(expand [4 2 2 1 0 0])

