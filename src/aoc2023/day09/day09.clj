(ns aoc2023.day09.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;(def fname "day09/test.txt")

(def fname "day09/part1.txt")

(defn strlist->intlist
  [st]
  (->> (s/split st #"\s+")
       (mapv #(Long/parseLong %))))

(defn read-file
  [fname]
  (->>
    (slurp (io/resource fname))
    s/split-lines
    (mapv strlist->intlist)))

;(read-file fname)

(defn seq->seqs*
  [sq]
  (->> (partition 2 1 sq)
       (mapv (fn [[f s]] (- s f)))))

(defn allzero? [sq]
  (= #{0} (set sq)))

(defn seq->seqs
  [sq]
  (let [[l1 l2] (split-with (complement allzero?)
                            (iterate seq->seqs* sq))]
    (concat l1 (list (first l2)))))

(defn next-entry
  [sq]
  (->> (seq->seqs sq)
       (map last)
       reverse
       (reductions +)
       last
       ;(mapv (comp vec list))
       ;reverse
       ;(mapv into (seq->seqs [10 13 16 21 30 45]))
       ))

(reduce + (map next-entry (read-file fname)))