(ns aoc2019.core-test
  (:require [clojure.test :refer :all]
            [aoc2019.core :refer :all]))

(deftest test-run-int-code-examples
  (testing "examples from aoc"
    (are [start-state _ end-state] (= end-state (run-int-code start-state))
      [] :causes :error-no-op
      [3] :causes :error-bad-op
      [99] :becomes [99]
      [1,0,0,0,99] :becomes [2,0,0,0,99]
      [2,3,0,3,99] :becomes [2,3,0,6,99]
      [2,4,4,5,99,0] :becomes [2,4,4,5,99,9801]
      [1,1,1,4,99,5,6,0,99] :becomes [30,1,1,4,2,5,6,0,99])))
