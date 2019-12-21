(ns aoc2019.day-8
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc2019/day_8.txt"))

(def dimensions [25 6])

(defn count-char [ch str]
  (count (filter #(= ch %) str)))

(defn part-1 [input dimensions]
  (let [layer-size (apply * dimensions)
        input-size (count input)
        layers (partition layer-size input)
        counts (map (juxt
                      (partial count-char \0)
                      (partial count-char \1)
                      (partial count-char \2)) layers)]
    (->> counts
      (sort-by first)
      first
      rest
      (apply *)
      )))

(defn rotate
  "transform list of layers (of pixels) into list of pixels (in layers)"
  [coll]
  (apply map list coll))

(defn part-2 [input dimensions]
  (let [layer-size (apply * dimensions)
        input-size (count input)
        layers (partition layer-size input)]
    (->> layers
      rotate
      (map #(first (filter #{\0 \1} %))) ;; preserve first non-transparent pixel
      (replace {\1 \# \0 \ }) ;; translate 0/1 for nicer display
      (partition (first dimensions)) ;; slice into lines
      (cons "") ;; add a blank line for presentation
      (map str/join) ;; join chars into lines
      (str/join "\n")))) ;; join lines into one string


(comment

  (part-1 input dimensions)

  (println (part-2 input dimensions))

  )
