(ns aoc2023.day02.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; read input

(defn readline
  [line]
  (-> line
      (s/replace "," "")
      (s/replace ";" "")
      (subs (+ (s/index-of line ":") 2))
#_  (->> (subs line (s/index-of ";" line))
    ;(s/split line #";")
       ;(drop 2)
       ;(map #(s/replace % "," ""))
       ;(map #(s/replace % ";" ""))
       ;(partition 2)
       ;(map reverse)
       )))

  (->> (slurp (io/resource "day02/test.txt"))
       s/split-lines
       (map readline)
       (map #(s/split % #" "))
       (map #(partition 2 %))
       (map #(map (fn [[f s]] {(keyword s) (Integer/parseInt f)}) %))
       (map #(apply merge-with max %)))
