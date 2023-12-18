(ns aoc2023.day05.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

; part 1

(defn str->nums
  [st]
  (let [st (s/trim st)]
    (when (not= st "")
      (->> (s/split st #"\s+")
           (mapv #(Long/parseLong %))))))

(comment

  (str->nums "1 2 3")

  (str->nums "1 2 3 4 5 7")

  (str->nums "12 23 34 45 56 78")

  (str->nums "")

  (str->nums " ")

  (str->nums "   ")

  (str->nums "      ")

  (str->nums "1 ")

  (str->nums " 1")

  (str->nums " 1 ")

  (str->nums "  3              6              8            999")

  )

(defn triple->fn
  [[t1 t2 t3]]
  (fn [n]
    (when (< (dec t2) n (+ t2 t3))
      (- n (- t2 t1)))))

(comment

  ((triple->fn (str->nums "199 98 2")) 97)

  ((triple->fn (str->nums "199 98 2")) 98)

  ((triple->fn (str->nums "199 98 2")) 99)

  ((triple->fn (str->nums "199 98 2")) 100)

  ((triple->fn (str->nums "52 50 48")) 50)

  )

(defn extract-line
  [st]
  (cond
    (s/starts-with? st "seeds:") [:seeds (str->nums (subs st 7))]
    (s/ends-with? st  " map:") (-> (s/split st #" ")
                                   first
                                   keyword)
    (= st "") nil
    (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} (first st)) (str->nums st)
    :else (throw (ex-info "Unknown input format " {:line st}))))

(defn input->map
  "Process the input file and get an in-map"
  [fname]
  (->> (slurp (io/resource fname))
       s/split-lines
       (keep extract-line)
       flatten
       (partition-by keyword?)
       (partition 2)
       (map (fn [[[f] s]]
              (if (= :seeds f)
                [f (vec s)]
                [f (vec (map (fn [triple] {:triple    (vec triple)
                                           :triple-fn (triple->fn triple)}) (partition 3 s)))])))
       (into {})))

(def in-map (input->map "day05/test.txt"))

(defn fns-for
  "Get all the triple fns from in-map for the given kwd."
  [in-map kwd]
  (->> (kwd in-map)
       (mapv :triple-fn)))

; :seed-to-soil
; :soil-to-fertilizer
; :fertilizer-to-water
; :water-to-light
; :light-to-temperature
; :temperature-to-humidity
; :humidity-to-location
(def fns-for-in-map (fns-for in-map :light-to-temperature))

(defn mapping-for
  "Apply all the fns for kwd in the in-map to the seed"
  [in-map kwd seed]
  ((apply juxt (fns-for in-map kwd)) seed))

(
 (mapping-for in-map :light-to-temperature 555)
  )

(defn mapping-for-seeds-and-kwds
  "Apply all the fns for kwd in the in-map to each of the seeds in the in-map"
  [in-map]
  (for [seed (:seeds in-map)
        kwd (keys (dissoc in-map :seeds))]
    (do (println seed kwd)
        [seed kwd (mapping-for in-map kwd seed)])

    ))

(mapping-for-seeds-and-kwds in-map)

;(let [in-map (dissoc in-map :seeds)
;      in-map-keys (keys in-map)]
;  (apply min (filter some? (mapcat #(mapping-for in-map % 15) in-map-keys))))
