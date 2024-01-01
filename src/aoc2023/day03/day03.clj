(ns aoc2023.day03.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(defn read-file
  [fname]
  (-> (slurp (io/resource fname))
      s/split-lines))

(def file-data (read-file "day03/test.txt"))

(defn width
  [file-data]
  (count (first file-data)))

;(comment
;  (width file-data)
;  )

(defn coords
  [file-data]
  (for [r (range)
        c (range (width file-data))]
    [r c]))

(defn digit?
  [ch]
  (boolean (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} ch)))

(defn space?
  [ch]
  (= \. ch))

(defn sym?
  [ch]
  (not (or (digit? ch)
           (space? ch))))

(defn file-data->map
  [file-data]
  (map
    (fn [coord char] {:coord coord
                      :char char
                      :token-type (cond
                                    (digit? char) :digit
                                    (sym? char) :sym
                                    (space? char) :space)})
    (coords file-data)
    (apply str file-data)))

(def file-data-map (file-data->map file-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn surrounds
  [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y] [x y] [(inc x) y]
   [(dec x) (inc y)] [x (inc y)][(inc x) (inc y)]])

(defn tokens
  [line]
  (let [token-type (first (map :token-type line))]
    (hash-map
      :token-type token-type
      :coords (case token-type
                :space (set (map :coord line))
                :digit (set (map :coord line))
                :sym (set (surrounds (first (map :coord line)))))
      :token (str (reduce str (map :char line))))))

(defn tokenise
  [file-data-map]
  (->> (partition-by :token-type file-data-map)
       (map tokens)))

(tokenise file-data-map)

(defn token-types
  [file-data-map]
  (->> (tokenise file-data-map)
       (group-by :token-type)))

(token-types file-data-map)
