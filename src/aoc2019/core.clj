(ns aoc2019.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn run-int-code
  "run the list of instructions through int-code computer"
  ([list] (run-int-code 0 list))
  ([pos list]
   (let [instructions (->> list (drop pos) (take 4))
         [op pa pb po] instructions
         a (get list pa)
         b (get list pb)]
     (cond
       (nil? op) :error-no-op
       (= op 99) list
       (= op 1) (recur (+ 4 pos) (assoc list po (biginteger (+' a b))))
       (= op 2) (recur (+ 4 pos) (assoc list po (biginteger (*' a b))))
       :else :error-bad-op
       ))))
