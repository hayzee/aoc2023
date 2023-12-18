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

(defn game-data
  [st]
  (let [[gid & gdat ] (-> st
                          (s/replace "," "")
                          (s/replace "Game " "")
                          (s/replace ":" "")
                          (s/replace ";" "")
                          (s/split #" "))]
    (merge {:gid (Integer/parseInt gid)}
           (->> (partition 2 gdat)
                (mapv (fn [[f s]] {(keyword s) (Integer/parseInt f)}))
                (reduce (partial merge-with max))))))

(comment
  (game-data "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (game-data "Game 1: 9 blue, 4 red; 1 red, 80 green, 6 blue; 2 green; -2 red"))

(def bag {:red 12
                :green 13
                :blue 14})

(defn possible-games
  [fname]
  (->> (slurp (io/resource fname))
       s/split-lines
       (map game-data)
       (map (fn [game] (assoc game :playable (not (or (> (:red game) (:red bag))
                                                  (> (:blue game) (:blue bag))
                                                  (> (:green game) (:green bag))
                                                  )))))
       (filter (fn [game] (:playable game)))
       (map :gid)
       (reduce +)
       ))

(possible-games "day02/test.txt")

(possible-games "day02/part1.txt")


; part 2


(defn min-cubes
  [fname]
  (->> (slurp (io/resource fname))
       s/split-lines
       (map game-data)))

(reduce + (map (fn [game] (reduce * ((juxt :blue :red :green) game))) (min-cubes "day02/test.txt")))

(reduce + (map (fn [game] (reduce * ((juxt :blue :red :green) game))) (min-cubes "day02/part1.txt")))
