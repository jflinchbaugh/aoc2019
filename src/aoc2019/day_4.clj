(ns aoc2019.day-4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (range 124075 580770))

(defn parse [s] (map (comp biginteger str) (str s)))

(defn freqs [lst]
  (let [s (->> lst (group-by identity) (map (fn [[k v]] [k (count v)])))]
    s)
  )

(defn good-for-1?
  "is it a good candidate for part 1"
  [c]
  (let [lst (parse c)]
    (and
      (apply <= lst)
      (->> lst freqs (some #(-> % second (>= 2)))))))

(defn good-for-2?
  "is it a good candidate for part 2"
  [c]
  (and
    (good-for-1? c)
    (->> c parse freqs (some #(-> % second (= 2))))))

(defn part-1 []
  (->> input (filter good-for-1?) count))

(defn part-2 []
  (->> input (filter good-for-2?) count))

(comment

  (part-1)

  (part-2)

  

)
