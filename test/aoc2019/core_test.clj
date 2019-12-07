(ns aoc2019.core-test
  (:require [clojure.test :refer :all]
            [aoc2019.core :refer :all]))

(deftest test-run-int-code-examples
  (testing "examples from aoc day 2"
    (are [start-state _ end-state] (= end-state (run-int-code start-state))
      [] :causes :error-no-op
      [98] :causes :error-bad-op
      [99] :becomes [99]
      [1,0,0,0,99] :becomes [2,0,0,0,99]
      [2,3,0,3,99] :becomes [2,3,0,6,99]
      [2,4,4,5,99,0] :becomes [2,4,4,5,99,9801]
      [1,1,1,4,99,5,6,0,99] :becomes [30,1,1,4,2,5,6,0,99]))

  (testing "error states in output"
    (are [start-state _ outputs] (= outputs (run-int-code start-state []))
      [] :outputs [:error-no-op]
      [98] :outputs [:error-bad-op]
      [104,10] :outputs [10 :error-no-op]
      [104,10,98] :outputs [10 :error-bad-op]
      ))

  (testing "examples from aoc day 5 - IO"
    (are [initial-state _ inputs _ outputs]
        (= outputs (run-int-code initial-state inputs))
      [3,0,4,0,99] :with [1] :outputs [1]
      [3,0,3,1,1,0,1,0,4,0,99] :with [10,12] :outputs [22]
      ))

  (testing "examples from aoc day 5 - parameter modes"
    (are [initial-state _ inputs _ outputs]
        (= outputs (run-int-code initial-state inputs))
      [1101,100,-1,0,4,0,99] :with [] :outputs [99]
      ))

  (testing "examples from aoc day 5/part 2 - flow control"
    (are [initial-state _ inputs _ outputs]
        (= outputs (run-int-code initial-state inputs))
      [3,9,8,9,10,9,4,9,99,-1,8] :with [8] :outputs [1]
      [3,9,8,9,10,9,4,9,99,-1,8] :with [7] :outputs [0]
      [3,9,7,9,10,9,4,9,99,-1,8] :with [8] :outputs [0]
      [3,9,7,9,10,9,4,9,99,-1,8] :with [7] :outputs [1]
      [3,3,1108,-1,8,3,4,3,99]   :with [8] :outputs [1]
      [3,3,1108,-1,8,3,4,3,99]   :with [7] :outputs [0]
      [3,3,1107,-1,8,3,4,3,99]   :with [8] :outputs [0]
      [3,3,1107,-1,8,3,4,3,99]   :with [7] :outputs [1]

      [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]:with [0] :outputs [0]
      [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]:with [2] :outputs [1]
      [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]:with [0] :outputs [0]
      [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]:with [2] :outputs [1]

      ))

    (testing "day 7 - IO with continuation when input not available"
      (is (= {:continue [2 [10,0,3,1,1,0,1,0,4,0,99] [] []]
              :outputs []
              :state [10,0,3,1,1,0,1,0,4,0,99]}
            (run-int-code 0 [3,0,3,1,1,0,1,0,4,0,99] [10] [])))
      (is (= [22]
            (:outputs
             (apply
               run-int-code
               [2 [10,0,3,1,1,0,1,0,4,0,99] [12] []]))))))
