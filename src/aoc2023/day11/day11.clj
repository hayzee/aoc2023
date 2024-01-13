(ns aoc2023.day11.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(def fname-test "day11/test.txt")

(def fname "day11/part1.txt")

(defn read-file
  [fname]
  (->> (slurp (io/resource fname))
       s/split-lines
       (mapv (comp vec seq))))

(defn transpose
  [mtx]
  (apply mapv vector mtx))

(defn blankline?
  [line]
  (every? #{\.} line))

(defn v-expand
  [mtx]
  (when (seq mtx)
    (let [line (first mtx)]
      (if (blankline? line)
        (cons line
              (cons
                line
                (v-expand (rest mtx))))
        (cons
          line
          (v-expand (rest mtx)))))))

(defn expand-universe
  [mtx]
  (-> mtx
      v-expand
      transpose
      v-expand
      transpose))

(defn num-cols
  [mtx]
  (count (first mtx)))

(defn num-rows
  [mtx]
  (count mtx))

(defn find-galaxies
  [mtx]
  (for [row (range (num-rows mtx))
        col (range (num-cols mtx))
        :when (not= \. (get-in mtx [row col]))]
    [row col]))

(defn cross-product
  [sq]
  (for [s1 sq
        s2 sq]
    [s1 s2]))

(defn distance
  [[[ar ac] [br bc]]]
  (+ (abs (- ar br))
     (abs (- ac bc))))

; part 1

(/ (reduce + (map distance (cross-product (find-galaxies (expand-universe (read-file fname))))))
   2)

