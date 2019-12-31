(ns aoc2019.core
  (:require [clojure.string :as str]))

(defn name-for-number-op [op]
  (let [ops {99 :halt,
             1 :add,
             2 :multiply,
             3 :in,
             4 :out,
             5 :jump-if-true
             6 :jump-if-false
             7 :less-than
             8 :equals
             9 :move-base}]
    (if op (ops (mod op 100)))))

(defn mode
  "calculate the parameter mode of an operation"
  [op pos]
  (get [:position :immediate :relative] (if op (-> op (/ pos) int (mod 10)))))

(defn grow
  "grow a vector to a max size, initializing the new spaces with 0"
  [vector & positions]
  (vec
    (take
      (apply max (cons (count vector) (map inc positions)))
      (concat vector (repeat 0)))))

(defn get-addr
  "calculate an address from base and val for a mode"
  [base mode val]
  (+ (if (= :relative mode) base 0) val))

(defn get-val [state base mode val]
  (if (= :immediate mode)
   val
   (let [pos (+ (if (= :relative mode) base 0) (if (nil? val) 0 val))
         ret (get state pos)]
     (if (nil? ret) 0 ret))))

(defn grow-assoc
  "assoc, but also grow to fit the index"
  [c index v]
  (assoc (grow c index) index v))

(defn run-int-code
  "run the int-code computer on a given machine state with inputs and outputs"
  ([state] (:state (run-int-code 0 0 state [] [])))
  ([state inputs] (:outputs (run-int-code 0 0 state inputs [])))
  ([pos state inputs outputs] (run-int-code pos 0 state inputs outputs))
  ([pos base state inputs outputs]
   (let [instructions (->> state (drop pos) (take 4))
         [op pa pb pc] instructions
         op-name (name-for-number-op op)
         ma (mode op 100)
         mb (mode op 1000)
         mc (mode op 10000)
         a (get-val state base ma pa)
         b (get-val state base mb pb)
         c (get-val state base mc pc)]
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
       (recur
         (+ 4 pos)
         base
         (grow-assoc state (get-addr base mc pc) (+' a b))
         inputs
         outputs)

       (= op-name :multiply)
       (recur
         (+ 4 pos)
         base
         (grow-assoc state (get-addr base mc pc) (*' a b))
         inputs
         outputs)

       (= op-name :in )
       (if (empty? inputs)
         {:continue [pos state inputs outputs] :state state :outputs outputs}
         (recur
           (+ 2 pos)
           base
           (grow-assoc state (get-addr base ma pa) (first inputs))
           (rest inputs)
           outputs))

       (= op-name :out)
       (recur (+ 2 pos) base state inputs (conj outputs a))

       (= op-name :jump-if-true)
       (recur (if (zero? a) (+ 3 pos) b) base state inputs outputs)

       (= op-name :jump-if-false)
       (recur (if-not (zero? a) (+ 3 pos) b) base state inputs outputs)

       (= op-name :less-than)
       (recur
         (+ pos 4)
         base
         (grow-assoc state (get-addr base mc pc) (if (< a b) 1 0))
         inputs
         outputs)

       (= op-name :equals)
       (recur
         (+ pos 4)
         base
         (grow-assoc state (get-addr base mc pc) (if (= a b) 1 0))
         inputs
         outputs)

       (= op-name :move-base)
       (recur (+ pos 2) (+ a base) state inputs outputs)

       ))))

(defn map-val
  "apply a fn to values of a hashmap"
  [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn load-int-code [file-name]
  (->
    file-name
    slurp
    str/trim
    (str/split #",")
    (->>
      (map (comp int biginteger)))))
