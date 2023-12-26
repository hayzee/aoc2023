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

; (width file-data)

(defn coords
  [file-data]
  (for [r (range)
        c (range (width file-data))]
    [r c]))

(defn file-data->map
  [file-data]
  (map
   (fn [coord char] {:coords [coord] :chars [char]})
   (coords file-data)
   (apply str file-data)))

(def file-data-map (file-data->map file-data))

(defn digit?
  [ch]
  (boolean (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} ch)))

(defn sym?
  [ch]
  (not (or (digit? ch)
           (= \. ch))))

(defn extract-numbers
  [file-data-map]
  (->> (partition-by (fn [me] (digit? (first (:chars me)))) file-data-map)
       (filter (fn [maps] (digit? (first (:chars (first maps))))))
       (map (fn [maps] (-> (apply merge-with concat maps)
                           (update :chars #(Integer/parseInt (apply str %)))
                           (st/rename-keys {:chars :num})
                           (update :coords set))))))

(extract-numbers file-data-map)

(defn extract-symbols
  [file-data-map]
  (->> (partition-by (fn [me] (sym? (first (:chars me)))) file-data-map)
       (filter (fn [maps] (sym? (first (:chars (first maps))))))
       (map (fn [maps] (-> (apply merge-with concat maps)
                           ;(update :chars #(Integer/parseInt (apply str %)))
                           (st/rename-keys {:chars :num})
                           (update :coords set))))))

(extract-symbols file-data-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
