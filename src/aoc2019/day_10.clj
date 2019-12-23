(ns aoc2019.day-10
  (:require [clojure.string :as str]))

(def asteroids "
##.###.#.......#.#....#....#..........#.
....#..#..#.....#.##.............#......
...#.#..###..#..#.....#........#......#.
#......#.....#.##.#.##.##...#...#......#
.............#....#.....#.#......#.#....
..##.....#..#..#.#.#....##.......#.....#
.#........#...#...#.#.....#.....#.#..#.#
...#...........#....#..#.#..#...##.#.#..
#.##.#.#...#..#...........#..........#..
........#.#..#..##.#.##......##.........
................#.##.#....##.......#....
#............#.........###...#...#.....#
#....#..#....##.#....#...#.....#......#.
.........#...#.#....#.#.....#...#...#...
.............###.....#.#...##...........
...#...#.......#....#.#...#....#...#....
.....#..#...#.#.........##....#...#.....
....##.........#......#...#...#....#..#.
#...#..#..#.#...##.#..#.............#.##
.....#...##..#....#.#.##..##.....#....#.
..#....#..#........#.#.......#.##..###..
...#....#..#.#.#........##..#..#..##....
.......#.##.....#.#.....#...#...........
........#.......#.#...........#..###..##
...#.....#..#.#.......##.###.###...#....
...............#..#....#.#....#....#.#..
#......#...#.....#.#........##.##.#.....
###.......#............#....#..#.#......
..###.#.#....##..#.......#.............#
##.#.#...#.#..........##.#..#...##......
..#......#..........#.#..#....##........
......##.##.#....#....#..........#...#..
#.#..#..#.#...........#..#.......#..#.#.
#.....#.#.........#............#.#..##.#
.....##....#.##....#.....#..##....#..#..
.#.......#......#.......#....#....#..#..
...#........#.#.##..#.#..#..#........#..
#........#.#......#..###....##..#......#
...#....#...#.....#.....#.##.#..#...#...
#.#.....##....#...........#.....#...#...
")

(defn line->flags
  "input line to flags"
  [line]
  (vec (map #(= \# %) line)))

(defn parse [asteroid-map]
  (-> asteroid-map
      str/trim
      (str/split #"\n")
      (->> (map (comp line->flags str/trim)))
      vec))

(defn coordinate-list [grid]
  (let [height (count grid)
        width (-> grid first count)
        xs (range width)
        ys (range height)]
    (for [y ys x xs]
      [[x y] (-> grid (get y) (get x))])))

(defn sq-distance [origin destination]
  (let [dx (- (first destination) (first origin))
        dy (- (second destination) (second origin))]
    (+ (* dx dx) (* dy dy))))

(defn direction
  "encode direction into [direction-x direction-y slope]"
  [origin destination]
  (let [dx (- (first destination) (first origin))
        dy (- (second destination) (second origin))]
    [(compare dx 0) (compare dy 0) (if (zero? dx) :inf (/ dy dx))]))

(defn direction->angle [[dx dy slope]]
  (cond
    (= [0 0] [dx dy]) nil
    (= [0 -1 :inf] [dx dy slope]) 0.0
    (= [1 -1] [dx dy]) (+ (* 1/2 Math/PI) (Math/atan slope))
    (= [1 0 0] [dx dy slope]) (* Math/PI 1/2)
    (= [1 1] [dx dy]) (+ (* 1/2 Math/PI) (Math/atan slope))
    (= [0 1 :inf] [dx dy slope]) Math/PI
    (= [-1 1] [dx dy]) (+ (* 3/2 Math/PI) (Math/atan slope))
    (= [-1 0 0] [dx dy slope]) (* Math/PI 3/2)
    (= [-1 -1] [dx dy]) (+ (* 3/2 Math/PI) (Math/atan slope))
    ))

(defn lines
  "compute unique lines by direction to a list of destinations in each direction"
  [o ds]
  (group-by (partial direction o) ds))

(defn asteroid-locations
  [asteroid-map]
  (->>
   asteroid-map
   parse
   coordinate-list
   (filter second)
   (map first)))

(defn optimal-location
  "find optimal location for an asteroid base"
  [asteroid-map]
  (let [ls (asteroid-locations asteroid-map)]
    (->> (for [o ls]
           [o (->> ls
                   (remove #{o})
                   (lines o))])
      (sort-by (comp count keys second))
      last)))

(defn unwind [lines]
  (let [lc (count lines)
        mac (apply max (map count lines))]
    (remove nil?
      (for [shell (range mac)
            dir (range lc)]
        (-> lines vec (get dir) vec (get shell))))))

(defn encode [a]
  (+ (* 100 (first a)) (second a)))

(defn part-1 []
  (-> asteroids
    optimal-location
    second
    count))

(defn part-2 []
  (let [op-loc (->> asteroids optimal-location)
        origin (->> op-loc first)]
    (->> op-loc
      second
      (sort-by (comp direction->angle first))
      (map (comp (partial sort-by (partial sq-distance origin)) second))
      unwind
      (drop 199)
      first
      encode
      )
    ))

(comment

  (optimal-location asteroids)

  (part-1)

  (part-2)

  )
