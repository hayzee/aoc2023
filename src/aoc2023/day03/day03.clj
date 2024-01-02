(ns aoc2023.day03.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(defn read-file
  [fname]
  (-> (slurp (io/resource fname))
      s/split-lines))

;(def file-data (read-file "day03/test.txt"))

(def file-data (read-file "day03/part1.txt"))

(defn width
  [file-data]
  (count (first file-data)))

;(width file-data)

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
                                    (sym? char) :symbol
                                    (space? char) :space)})
    (coords file-data)
    (apply str file-data)))

(def file-data-map (file-data->map file-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn surrounds
  [[x y]]
  (for [x (range (dec x) (+ x 2))
        y (range (dec y) (+ y 2))]
    [x y]))

(defn tokens
  [line]
  (let [token-type (first (map :token-type line))]
    (hash-map
      :token-type token-type
      :coords (case token-type
                :space (set (map :coord line))
                :digit (set (map :coord line))
                :symbol (set (surrounds (first (map :coord line)))))
      :token (str (reduce str (map :char line))))))

(defn tokenise
  [file-data-map]
  (->> (partition-by :token-type file-data-map)
       (map tokens)))

;(tokenise file-data-map)

(defn token-types
  [file-data-map]
  (let [ttypes (->> (tokenise file-data-map)
                    (group-by :token-type))]
    (st/rename-keys ttypes
                    {:digit :digits
                     :space :spaces
                     :symbol :symbols})))

;(token-types file-data-map)

(defn overlaps
  [file-data-map]
  (let [ttypes (token-types file-data-map)
       {:keys [digits symbols]} ttypes]
   (for [s symbols
         d digits
         :when (seq (st/intersection (:coords s) (:coords d)))]
     {:symbol s :digit d})))

(defn solve
  [file-data-map]
  (->> (overlaps file-data-map)
       (map (comp :token :digit))
       (map #(Integer/parseInt %))
       (reduce +)))

(solve file-data-map)

; part 2

(defn solve2
  [file-data-map]
  (->> (overlaps file-data-map)            ; find overlaps
       (group-by :symbol)                  ; group by common symbols
       (filter #(< 1 (count (second %))))  ; where the symbol is attached to >1 number
       (map (fn [[f s]] (reduce * (map (comp #(Integer/parseInt %) :token :digit) s)))) ; multiply the attached digits
       (reduce +)  ; and sum them
       ))

; job done!
(solve2 file-data-map)
