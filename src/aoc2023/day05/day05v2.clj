(ns aoc2023.day05.day05v2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; part 1

(defn intstr?
  [s]
  (if (= s "")
    false
    (not (seq (keep #(when (not (Character/isDigit ^char %)) %) s)))))

(defn read-file
  [fname]
  (->> (slurp (io/resource fname))
       (s/split-lines)
       (mapcat (fn [line] (s/split line #" ")))
       (remove (fn [line] (#{"" "map:"} line)))
       (partition-by intstr?)
       (partition 2)
       (mapv (fn [[f s]]
               (let [kwd (keyword (s/replace (first f) ":" ""))
                     nvec (mapv #(Long/parseLong %) s)]
                 [kwd
                  (condp = kwd
                    :seeds (mapv #(Long/parseLong %) s)
                    kwd (mapv vec (partition 3 nvec)))])))
       (into {})))

(read-file "day05/test.txt")

(defn triple->mapfn
  [[t1 t2 t3]]
  (fn [i]
    (if (<= t2 i (+ t2 t3))
      (- i (- t2 t1))
      i)))
