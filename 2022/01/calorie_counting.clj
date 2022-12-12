(require '[clojure.string :as str])

(defn sum [coll]
  (reduce + 0 coll))

(def descending-calories-sum
  (as-> (slurp "input.txt") input
    (str/split input #"\n\n")
    (map str/split-lines input)
    (map (fn [xs] (map #(Integer/parseInt %) xs)) input)
    (map sum input)
    (sort #(compare %2 %1) input)))

(def part-1 (first descending-calories-sum))
;; => 71502

(def part-2 (sum (take 3 descending-calories-sum)))
;; => 208191
