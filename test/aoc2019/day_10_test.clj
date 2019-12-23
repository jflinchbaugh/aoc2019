(ns aoc2019.day-10-test
  (:require [aoc2019.day-10 :refer :all]
            [clojure.test :refer :all]))

(deftest test-sq-distance
  (testing "sq-distance"
    (are [sd o d]
        (and 
          (= sd (sq-distance o d))
          (= sd (sq-distance d o)))
      2 [0 0] [1 1]
      1 [0 0] [1 0]
      117 [3 6] [12 0]
      )))

(deftest test-direction
  (testing "direction"
    (are [dir o d]
        (= dir (direction o d))
        [0 0 :inf] [0 0] [0 0]
        [1 1 2/1] [0 0] [1 2]
        [1 1 1/2] [0 0] [2 1]
        [-1 -1 2/1] [0 0] [-1 -2]
        [-1 -1 1/2] [0 0] [-2 -1]
        [1 -1 -2/1] [0 0] [1 -2]
        [-1 1 -2/1] [0 0] [-1 2]
        [1 1 2/1] [0 0] [2 4]
        [1 1 2/1] [0 0] [4 8]
        [1 0 0] [0 0] [10 0]
        [0 1 :inf] [0 0] [0 10]
        )))

(deftest test-lines
  (testing "lines"
    (are [res o ds]
      (= res (lines o ds))
      {[1 0 0] [[1 0]]} [0 0] [[1 0]]
      {[1 0 0] [[1 0] [2 0]]} [0 0] [[1 0] [2 0]]
      {[1 0 0] [[1 0]] [0 1 :inf] [[0 1]]} [0 0] [[1 0] [0 1]]
      )))

(deftest test-optimal-location
  (testing "optimal-location"
    (let [res (optimal-location "
      .#..#
      .....
      #####
      ....#
      ...##
      ")]
      (is (= [3 4] (first res)))
      (is (= 8 (count (second res)))))))

(deftest test-direction->angle
  (testing "direction->sample to origin is nil"
    (is (nil? (direction->angle [0 0 :inf]))))
  (testing "direction->angle"
    (are [angle d]
        (= (* angle Math/PI 2) (direction->angle d))
      0 [0 -1 :inf]
      1/8 [1 -1 -1]
      1/4 [1 0 0]
      3/8 [1 1 1]
      1/2 [0 1 :inf]
      5/8 [-1 1 -1]
      3/4 [-1 0 0]
      7/8 [-1 -1 1]
      )))

(deftest test-unwind
  (testing "unwwind"
    (is (= [1 3 5 2 4] (unwind '((1 2) (3 4) (5)))))))
