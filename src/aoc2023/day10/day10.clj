(ns aoc2023.day10.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as st]))

(def fname-test "day10/test.txt")

(def fname-test2 "day10/test2.txt")

(def fname "day10/part1.txt")

(defn read-board
  [fname]
  (->> (io/resource fname)
       slurp
       s/split-lines
       (mapv (comp vec seq))))


(def board (read-board fname-test))

(defn display-board
  [board]
  (mapv #(println (apply str %)) board)
  (println "============================================"))

(display-board board)

(defn width
  [board]
  (count (first board)))

(defn height
  [board]
  (count board))

(defn find-start
  [board]
  (-> (for [r (range (height board))
            c (range (width board))
            :when (= \S (get-in board [r c]))]
        [r c])
      first))

(def start-pos (find-start board))

(def char-moves
  {

   \F [[1 0] [0 1]]
   \7 [[1 0] [0 -1]]
   \L [[-1 0] [0 1]]
   \J [[-1 0] [0 -1]]
   \| [[-1 0] [1 0]]
   \- [[0 -1] [0 1]]
   \S [[1 0]]   ; assume down is first move ('cos it is in both of my files).
   })

(defn vector-add
  [v1 v2]
  (mapv + v1 v2))

(defn make-move
  ; get char at current position `pos`
  ; identify moves for char
  ; vector add each move to current position to create 2 new candidate positions
  ; discard the one which is already in `positions`
  ; recurse with the other - i.e board, new position, (conj positions pos)
  [board pos positions]

  (let [char (get-in board pos)
        moves (char-moves char)
        candidates (->> moves
                        (map #(vector-add pos %))
                        set)
        new-pos (-> (st/difference candidates (set positions))
                    first)]
    ;(clojure.pprint/pprint {:pos        pos
    ;                        :char       char
    ;                        :moves      moves
    ;                        :candidates candidates
    ;                        :positions  positions
    ;                        :move       new-pos})
    [new-pos (conj positions pos)]))

(make-move board start-pos [])
(make-move board [2 1] [[1 1]])
(make-move board [3 1] [[1 1] [2 1]])

(defn board-iterator
  [board]
  (fn [[pos positions]]
    (make-move board pos positions)))

;(second (last (take-while first (iterate (board-iterator board) [start-pos []]))))

(->
  (reductions (fn [a e] (assoc-in a e \*)) board (second (last (take-while first (iterate (board-iterator board) [start-pos []])))))
  ;(map display-board)
  count
  (/ 2))


;;


(let [[s1 s2] (split-with first (iterate (board-iterator board) [start-pos []]))]
  [s1
   (-> (first s2)
       second)])


;(drop-while first (iterate (board-iterator board) [start-pos []]))
