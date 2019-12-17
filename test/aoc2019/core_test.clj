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
      (let [x 1
            start-state [3,0,3,1,1,0,1,0,4,0,99]
            io-pause-state (assoc start-state 0 10)]
        (is (= {:continue [2 io-pause-state [] []]
                :outputs []
                :state io-pause-state}
              (run-int-code 0 start-state [10] []))
          "run with too little input, so it returns a paused state")
        (is (= [22]
              (:outputs
               (apply
                 run-int-code
                 [2 io-pause-state [12] []])))
          "run from the paused state, and it finishes")))

  (testing "examples from aoc day 9/part 1 - relative mode"
    (are [initial-state _ outputs]
        (= outputs (run-int-code initial-state []))
      [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
      :outputs [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

      [1102,34915192,34915192,7,4,7,99,0]
      :outputs [(* 34915192 34915192)]

      [104,1125899906842624,99]
      :outputs [1125899906842624]
      )))

(deftest test-grow
  (testing "grow list to a new size"
    (is (= [1 2 0] (grow [1 2] 2))))
  (testing "grow list to a max size"
    (is (= [1 2 0 0 0 0 0 0 0 0 0] (grow [1 2]  10 2))))
  (testing "don't grow list unnecessarily"
    (is (= [1 2 3 4] (grow [1 2 3 4] 2)))))

(deftest test-mode
  (testing "decode mode"
    (is (= :position (mode 1 100)))
    (is (= :immediate (mode 101 100)))
    (is (= :relative (mode 201 100)))))
