(ns aoc2023.day08.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn line->entry
  [line]
  (let [line (-> line
                 (s/replace "(" "")
                 (s/replace ")" "")
                 (s/replace "= " "")
                 (s/replace "," ""))
        [key left right] (s/split line #" ")]
    [(keyword key) {:L (keyword left) :R (keyword right)}]))

(defn read-file
  [fname]
  (let [[moves _ & network] (-> (slurp (io/resource fname))
                                s/split-lines)]
    {:moves (flatten (repeat (map keyword (map str moves))))
     :state {:current :AAA
             :network (into {} (mapv line->entry network))}}))

;(-> (read-file "day08/test2.txt")
;    (update :moves #(take 10 %)))

(defn make-move
  [state kwd-move]
  (-> state
      (assoc :current (kwd-move ((:current state) (:network state))))))

(defn solve
  [fname]
  (let [file-data (read-file fname)]
    (reductions (fn [a e] (let [new-state (make-move a e)]
                            (if (= :ZZZ (:current new-state))
                              (reduced (assoc new-state :reduced true))
                              new-state))) (:state file-data) (:moves file-data))))

;(dec (count (solve "day08/test.txt")))
;
;(dec (count (solve "day08/test2.txt")))

(dec (count (solve "day08/part1.txt")))
