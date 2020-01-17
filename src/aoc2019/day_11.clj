(ns aoc2019.day-11
  (:require [clojure.string :as str]
            [aoc2019.core :refer :all]))

(def program (load-int-code "src/aoc2019/day_11.txt"))

(defn move [[sx sy dir sc] turn]
  (let [new-dir (case [dir turn]
                  [:north 0] :west
                  [:north 1] :east
                  [:east 0] :north
                  [:east 1] :south
                  [:south 0] :east
                  [:south 1] :west
                  [:west 0] :south
                  [:west 1] :north)
        new-x (case new-dir
                :north sx
                :east (inc sx)
                :south sx
                :west (dec sx))
        new-y (case new-dir
                :north (dec sy)
                :east sy
                :south (inc sy)
                :west sy)]
    [new-x new-y new-dir]))

(defn same-loc
  "create a predicate that compares a given location to location another"
  [a]
  #(= (take 2 a) (take 2 %)))

(defn paint-n-move [[[x y d _] & rest-visited] [paint turn]]
  (let [next-loc (move [x y d 0] turn)
        next-color (->>
                     rest-visited
                     (filter (same-loc next-loc))
                     first
                     last
                     (#(or % 0)))]
    (cons (conj next-loc next-color) (cons [x y d paint] rest-visited)))
  )

(defn part-1 []
  (->> (run-int-code 0 program [0] []) :continue (#(assoc % 2 [0]))))

(comment

  (part-1)

  (paint-n-move [[0 0 :north 0]] [1 1])

  (paint-n-move [[0 0 :north 0] [0 0 :south 0] [1 0 :west 0]] [1 1])

  )
