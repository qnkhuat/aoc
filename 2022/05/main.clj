(require '[clojure.string :as str]
         '[clojure.set :as set])

(def stacks
  (map #(map char (str/upper-case %))
       ["hrbdzfls"
        "tbmzr"
        "zlchns"
        "scfj"
        "pghwrzb"
        "vjzgdnmt"
        "glnwfzpq"
        "mzr"
        "mclgvrt"]))

(defn parse-move-line
  [line]
  (let [l (-> line
              (str/replace "move " "")
              (str/replace "from " "")
              (str/replace "to " "")
              (str/split #" "))]
    (map #(Integer/parseInt %) l)))

(def move-lines
  (->> (slurp "input.txt")
       str/split-lines
       (drop 10)))

(def moves (map parse-move-line move-lines))

(defn do-move
  [stacks [amount from to]]
  (let [from-stack (nth from stacks)
        to-stack   (nth to stacks)
        stack (->> (nth from stacks)
                   (take amount)
                   reverse)]
    (-> (assoc-seq stack from ()))))

(take 3 (range 10))

(update)

(defn move
  [stack moves])

(defn assoc-seq [s i v]
  (map-indexed (fn [j x] (if (= i j) v x)) s))
