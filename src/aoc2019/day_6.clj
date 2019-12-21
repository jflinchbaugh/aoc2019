(ns aoc2019.day-6
  (:require [aoc2019.core :refer [map-val]]
            [clojure.set :as set]
            [clojure.string :as str]))

(def orbits (slurp "src/aoc2019/day_6.txt"))

(defn parse [in]
  (-> in
    str/trim
    (str/split #"\n")
    (->> (map #(str/split (str/trim %) #"\)")))))

(defn orbit-map
  "produce a map of objects to what they orbit"
  [in]
  (->> in parse (group-by second) (map-val (comp first first))))

(defn walk
  "walk the mapping to build a list of steps to the beginning of the graph"
  [mapping steps]
  (let [nxt (mapping (first steps))]
    (if
        (nil? nxt) steps 
        (recur mapping (cons nxt steps)))))

(defn paths
  "expand a simple orbit-map into a map to the full path to the graph origin"
  [orbit-map]
  (map-val (comp (partial walk orbit-map) list) orbit-map))

(defn part-1 [in]
  (let [mapping (orbit-map in)]
    (->>
      mapping
      paths
      (map-val count)
      vals
      (reduce +))))

(defn part-2 [in]
  (let [mapping (orbit-map in)
        pths (paths mapping)
        you-path (set (pths "YOU"))
        san-path (set (pths "SAN"))]
    (->>
      (set/union
        (set/difference you-path san-path)
        (set/difference san-path you-path))
      count)))

(comment

  (part-1 orbits)

  (part-2 orbits)

  )
