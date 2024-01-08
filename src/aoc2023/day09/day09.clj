(ns aoc2023.day09.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def fname-test "day09/test.txt")

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
       last))

; answer 1
(reduce + (map next-entry (read-file fname)))

; part 2

(defn prior-entry
  [sq]
  (->> (seq->seqs sq)
       (map first)
       reverse
       (reductions (fn [a e] (- e a)) 0)
       last))

; answer 2
(reduce + (map prior-entry (read-file fname)))
