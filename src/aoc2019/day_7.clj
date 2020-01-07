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
  ([pos pgm input]
   (run-int-code pos pgm input [])))

(defn run-series
  "run the series of computers in the given phases"
  [
   input
   [pos0 pos1 pos2 pos3 pos4]
   [pgm0 pgm1 pgm2 pgm3 pgm4]
   [ph0 ph1 ph2 ph3 ph4]]
  (let [
        run-0 (run-once pos0 pgm0 (remove nil? [ph0 input]))
        run-1 (run-once pos1 pgm1 (remove nil? [ph1 (-> run-0 :outputs first)]))
        run-2 (run-once pos2 pgm2 (remove nil? [ph2 (-> run-1 :outputs first)]))
        run-3 (run-once pos3 pgm3 (remove nil? [ph3 (-> run-2 :outputs first)]))
        run-4 (run-once pos4 pgm4 (remove nil? [ph4 (-> run-3 :outputs first)]))
        ]
    [run-0 run-1 run-2 run-3 run-4]))

(defn run-feedback [input positions pgms phases]
  (let [series (run-series input positions pgms phases)
        output (-> series last :outputs last)
        continues (map :continue series)
        next-positions (map first continues)
        next-states (map second continues)
        ]
    (if (nil? (last continues))
      series
      (recur output next-positions next-states []))))

(defn part-1 [pgm]
  (apply max (map (comp first :outputs last (partial run-series 0 (repeat 0) (repeat pgm))) all-phases)))

(defn part-2 [pgm]
  (-> (map
     (comp
       last
       :outputs
       last
       (partial run-feedback 0 (repeat 0) (repeat pgm)))
     all-loopback-phases)
    sort
    last))

(comment

  (part-1 program)

  (part-2 program)

  )
