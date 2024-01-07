(ns aoc2023.day05.day05v2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; part 1

;(def fname "day05/test.txt")
(def fname "day05/part1.txt")

(defn intstr?
  "True if s is an integer string"
  [s]
  (if (= s "")
    false
    (every? #(Character/isDigit ^char %) s)))

(defn triple->triplefn
  "Create a triplefn from a triple"
  [[t1 t2 t3]]
  (fn [i]
    (when (<= t2 i (dec (+ t2 t3)))
      (- i (- t2 t1)))))

(defn input-map
  [fname]
  (->> (slurp (io/resource fname))
       (s/split-lines)
       (mapcat (fn [line] (s/split line #" ")))
       (remove (fn [line] (#{"" "map:"} line)))
       (partition-by intstr?)
       (partition 2)
       (mapv (fn [[f s]]
               (let [kwd (keyword (s/replace (first f) ":" ""))
                     nvec (mapv #(Long/parseLong %) s)
                     triples (mapv vec (partition 3 nvec))]
                 [kwd
                  (if (= kwd :seeds)
                    nvec
                    {:triples triples
                     :triple-fns (map triple->triplefn triples)})])))
       (into {})))

(defn apply-fns
  [input-map kwd seed-val]
  (or (->> (:triple-fns (kwd input-map))
           (keep (fn [f] (f seed-val)))
           first)
      seed-val))

(defn apply-all-fns
  [input-map seed-val]
  [(->> seed-val
     (apply-fns input-map :seed-to-soil)
     (apply-fns input-map :soil-to-fertilizer)
     (apply-fns input-map :fertilizer-to-water)
     (apply-fns input-map :water-to-light)
     (apply-fns input-map :light-to-temperature)
     (apply-fns input-map :temperature-to-humidity)
     (apply-fns input-map :humidity-to-location))])

(let [inmap (input-map fname)]
 (apply min (mapcat #(apply-all-fns inmap %) (:seeds inmap))))
