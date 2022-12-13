(require '[clojure.string :as str]
         '[clojure.set :as set])

(def stacks
  (map #(map char (str/upper-case %))
       ["HRBDZFLS"
        "TBMZR"
        "ZLCHNS"
        "SCFJ"
        "PGHWRZB"
        "VJZGDNMT"
        "GLNWFSPQ"
        "MZR"
        "MCLGVRT"]))

#_(def stacks
    (map #(map char (str/upper-case %))
         ["zn"
          "mcd"
          "p"]))

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

(defn assoc-seq
  "inline replacement"
  [s i v]
  (map-indexed (fn [j x] (if (= i j) v x)) s))

(def moves (map parse-move-line move-lines))

#_(def moves [[1 2 1] [ 3 1 3 ] [ 2 2 1 ] [ 1 1 2]])

(defn do-move-1
  [stacks [amount from to :as move]]
  (let [from       (dec from)
        to         (dec to)
        from-stack (nth stacks from from)
        to-stack   (nth stacks to)
        new-stack  (->> from-stack
                        (take-last amount)
                        reverse
                        (concat to-stack))]
    (-> (assoc-seq stacks from (drop-last amount from-stack))
        (assoc-seq to new-stack))))

;; part 1
(->> (reduce do-move-1 stacks moves)
     (map last)
     str/join)
;; => "RNZLFZSJH"

(defn do-move-2
  [stacks [amount from to :as move]]
  (let [from       (dec from)
        to         (dec to)
        from-stack (nth stacks from from)
        to-stack   (nth stacks to)
        new-stack  (->> from-stack
                        (take-last amount)
                        (concat to-stack))]
    (-> (assoc-seq stacks from (drop-last amount from-stack))
        (assoc-seq to new-stack))))

(->> (reduce do-move-2 stacks moves)
     (map last)
     str/join)
;; => "CNSFCGJSM"
