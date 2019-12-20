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

(defn lines
  "compute unique lines by direction to a list of destinations in each direction"
  [o ds]
  (group-by (partial direction o) ds))

(defn asteroid-locations
  "find optimal location"
  [asteroid-map]
  (->>
    asteroid-map
    parse
    coordinate-list
    (filter second)
    (map first))
  )

(defn optimal-location [asteroid-map]
  (let [ls (asteroid-locations asteroid-map)]
    (->> (for [o ls]
           ;; [o (count (keys (lines o (remove #(= o %) ls))))]
           [o (->> ls
                (remove #{o})
                (lines o)
                keys
                count)])
      (sort-by second)
      last
      )))

(defn part-1 []
  (second (optimal-location asteroids))
  )

(comment

  (part-1)

  )
