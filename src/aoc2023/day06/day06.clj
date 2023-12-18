(ns aoc2023.day06.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; part 1

(def fname "day06/part1.txt")

(defn read-file
  [fname]
 (let [[s1 s2] (->> (slurp (io/resource fname))
                    s/split-lines
                    (mapv (fn [s] (s/split s #"\s+"))))]
   (->> (map vector s1 s2)
        rest
        (map (fn [[f s]] {:ms (Integer/parseInt f) :record (Integer/parseInt s)})))))

(def file-data (read-file "day06/part1.txt"))

(defn options
  [ms]
  (->>
    (map (fn [acc remain] {:acc      acc
                          :remain   remain
                          :distance (* acc remain)})
        (range 1 ms)
        (reverse (range 1 ms)))
    (map :distance)))

(defn augment-with-options
  [file-date]
  (->> file-data
      (map (fn [m] (assoc m :options (options (:ms m)))))))

(def file-data-aug-options (augment-with-options file-data))

(->> (map (fn [m] (assoc m :winners (filter #(> % (:record m)) (:options m)))) file-data-aug-options)
     (map :winners)
     (map count)
     (reduce *))




[[45988373 295173412781210]]

(take 10 (options 45988373))


(count (filter #(> % 295173412781210) (options 45988373)))