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
  [fname]
  (->> (slurp (io/resource fname))
      s/split-lines
      (keep extract-line)
      flatten
      ;     (fn [sq] (concat (first sq) (rest sq)))                ; a bit ugly, but flattens the first vector
      (partition-by keyword?)
      (partition 2)
      (map (fn [[[f] s]]
             (if (= :seeds f)
               [f (vec s)]
               [f (vec (map (fn [triple] {:triple    (vec triple)
                                          :triple-fn (triple->fn triple)}) (partition 3 s)))])))
       (into {})))

(defn fns-for
  [in-map kwd]
  (->> (kwd in-map)
       (mapv :triple-fn)))

(defn mapping-for
  [in-map kwd arg]
  ((apply juxt (fns-for in-map kwd)) arg))

(let [in-map (dissoc (input->map "day05/test.txt") :seeds)
      in-map-keys (keys in-map)]
  (apply min (filter some? (mapcat #(mapping-for in-map % 15) in-map-keys))))
