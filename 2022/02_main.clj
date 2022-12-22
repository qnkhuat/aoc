(require '[clojure.string :as str]
         '[clojure.core.match :refer [match]])

(def symbol->play
  {"A" :rock
   "X" :rock

   "B" :paper
   "Y" :paper

   "C" :scissors
   "Z" :scissors})

(defn sum
  [coll]
  (reduce + coll))

(def play->score
  {:rock     1
   :paper    2
   :scissors 3})

(defn score?
  "if y win ?"
  [[x y]]
  (if (= x y)
    3
    (match [x y]
      [:rock :paper]
      6
      [:rock :scissors]
      0

      [:paper :scissors]
      6
      [:paper :rock]
      0

      [:scissors :rock]
      6
      [:scissors :paper]
      0)))

(defn turn->score
  [turn]
  (+ (score? turn) (play->score (second turn))))

(def input (->> (slurp "02_input.txt")
                str/split-lines
                (map #(str/split % #" "))))

(def part1
  (->> input
      (map (fn [x] (map symbol->play x)))
      (map turn->score)
      sum))
;; => 14163

(defn turn-to-play
  [[x y]]
  (let [x (symbol->play x)]
    [x
     (case y
       ;; need to lose
       "X"
       (case x
         :rock :scissors

         :paper :rock

         :scissors :paper)

       ;; need to draw
       "Y"
       x

       ;; need to win
       "Z"
       (case x
         :rock :paper

         :paper :scissors

         :scissors :rock))]))

(def part2
  (->> input
      (map turn-to-play)
      (map turn->score)
      sum))
;; => 12091
