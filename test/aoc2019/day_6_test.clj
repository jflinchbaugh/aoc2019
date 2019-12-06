(ns aoc2019.day-6-test
  (:require [aoc2019.day-6 :refer :all]
            [clojure.test :refer :all]))

(deftest test-part-1
  (testing "total orbits"
    (is (= 42 (part-1 "
      COM)B
      B)C
      C)D
      D)E
      E)F
      B)G
      G)H
      D)I
      E)J
      J)K
      K)L
      ")))))

(deftest test-part-2
  (testing "orbit hops between YOU and SAN"
    (is (= 4 (part-2 "
      COM)B
      B)C
      C)D
      D)E
      E)F
      B)G
      G)H
      D)I
      E)J
      J)K
      K)L
      K)YOU
      I)SAN
      ")))))
