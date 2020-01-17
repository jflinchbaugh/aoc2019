(ns aoc2019.day-11-test
  (:require [aoc2019.day-11 :refer :all]
            [clojure.test :refer :all]))

(deftest test-move
  (testing "move"
    (are [dest start turn] (= dest (move start turn))
      [-1 0 :west] [0 0 :north] 0
      [1 0 :east]  [0 0 :north] 1
      [2 1 :east]  [1 1 :north] 1
      [1 0 :north] [1 1 :east] 0)))

(deftest test-paint-n-move
  (is (=
        [[1 0 :east 0] [0 0 :north 1]]
        (paint-n-move [[0 0 :north 0]] [1 1])))
  (is (=
        [[1 0 :east 1] [0 0 :north 1] [0 0 :south 0] [1 0 :west 1]]
        (paint-n-move [[0 0 :north 0] [0 0 :south 0] [1 0 :west 1]] [1 1]))))

(deftest test-same-loc
  (is (= true ((same-loc [1 0 :x :y]) [1 0 :z :q])))
  (is (= false ((same-loc [1 1 :x :y]) [1 0 :z :q]))))
