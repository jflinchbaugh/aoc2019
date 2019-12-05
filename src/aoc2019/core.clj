(ns aoc2019.core)

(defn name [op]
  (let [ops {99 :halt,
             1 :add,
             2 :multiply,
             3 :in,
             4 :out,
             5 :jump-if-true
             6 :jump-if-false
             7 :less-than
             8 :equals}]
    (if op (ops (mod op 100)))))

(defn mode [op pos]
  (get [:position :immediate] (if op (-> op (/ pos) int (mod 10)))))

(defn get-val [state mode val]
  (if (= :immediate mode)
    val
    (get state val)))

(defn run-int-code
  "run the int-code computer on a given machine state with inputs and outputs"
  ([state] (:state (run-int-code 0 state [] [])))
  ([state inputs] (:outputs (run-int-code 0 state inputs [])))
  ([pos state inputs outputs]
   (let [instructions (->> state (drop pos) (take 4))
         [op pa pb pc] instructions
         op-name (name op)
         ma (mode op 100)
         mb (mode op 1000)
         mc (mode op 10000)
         a (get-val state ma pa)
         b (get-val state mb pb)
         c (get-val state mc pc)]
     (cond
       ; exits for errors
       (nil? op)
       {:state :error-no-op :outputs (conj outputs :error-no-op)} 

       (nil? op-name)
       {:state :error-bad-op :outputs (conj outputs :error-bad-op)} 

       ; valid operations
       (= op-name :halt)
       {:state state :outputs outputs}

       (= op-name :add)
       (recur (+ 4 pos) (assoc state pc (+' a b)) inputs outputs)

       (= op-name :multiply)
       (recur (+ 4 pos) (assoc state pc (*' a b)) inputs outputs)

       (= op-name :in )
       (recur (+ 2 pos) (assoc state pa (first inputs)) (rest inputs) outputs)

       (= op-name :out)
       (recur (+ 2 pos) state inputs (conj outputs a))

       (= op-name :jump-if-true)
       (recur (if (zero? a) (+ 3 pos) b) state inputs outputs)

       (= op-name :jump-if-false)
       (recur (if-not (zero? a) (+ 3 pos) b) state inputs outputs)

       (= op-name :less-than)
       (recur (+ pos 4) (assoc state pc (if (< a b) 1 0)) inputs outputs)

       (= op-name :equals)
       (recur (+ pos 4) (assoc state pc (if (= a b) 1 0)) inputs outputs)

       ))))
