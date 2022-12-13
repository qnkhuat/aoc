(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn sum
  [coll]
  (reduce + 0 coll))

(defn char->score
  [char]
  (let [o (int char)]
   (if (>= o 97)
     ;;lower case
     (- o 96)
     (+ 27 (- o 65)))))

;; part 1
(->> (slurp "input.txt")
     str/split-lines
     (map #(partition (/ (count %) 2) %))
     (map #(map set %))
     (map (partial apply set/intersection))
     (map first)
     (map char->score)
     sum)
;; => 7701

;; part 2
(->> (slurp "input.txt")
     str/split-lines
     (map set)
     (partition 3)
     (map (fn [g] (apply set/intersection g)))
     (map first)
     (map char->score)
     sum)
;; => 2644
