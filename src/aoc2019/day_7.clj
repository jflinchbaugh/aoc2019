(ns aoc2019.day-7
  (:require [clojure.math.combinatorics :as combo]
            [aoc2019.core :refer :all]))

(def program (load-int-code "src/aoc2019/day_7.txt"))

(def all-phases
  (combo/permutations (range 5)))

(def all-loopback-phases
  (combo/permutations (range 5 10)))

(defn run-once
  "run program once through the int-code computer at a given phase and input"
  ([pos pgm phase input]
   (run-int-code pos pgm [phase input] [])))

(defn run-series
  "run the series of computers in the given phases"
  [[pos0 pos1 pos2 pos3 pos4] [pgm0 pgm1 pgm2 pgm3 pgm4] [ph0 ph1 ph2 ph3 ph4]]
  (let [
        run-0 (run-once pos0 pgm0 ph0 0)
        run-1 (run-once pos1 pgm1 ph1 (-> run-0 :outputs first))
        run-2 (run-once pos2 pgm2 ph2 (-> run-1 :outputs first))
        run-3 (run-once pos3 pgm3 ph3 (-> run-2 :outputs first))
        run-4 (run-once pos4 pgm4 ph4 (-> run-3 :outputs first))
        ]
    [run-0 run-1 run-2 run-3 run-4]))

(defn part-1 [pgm]
  (apply max (map (comp first #(% :outputs) (partial run-series (repeat 0) (repeat pgm))) all-phases)))

(comment

  (run-series (repeat 0) (repeat program) [0 1 2 3 4])

  (part-1 program)
;; => 117312
  )
