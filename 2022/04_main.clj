(require '[clojure.string :as str]
         '[clojure.set :as set])

(defn parse-pair
  [pair]
  (map #(Integer/parseInt %) (str/split pair #"-")))

(defn parse-line
  [s]
  (map parse-pair (str/split s #",")))

(defn is-inclusive?
  [[[s1 e1] [s2 e2]]]
  (or (and (<= s1 s2) (>= e1 e2)) (and (>= s1 s2) (<= e1 e2))))

;; part 1

(->> (slurp "04_input.txt")
     str/split-lines
     (map parse-line)
     (filter is-inclusive?)
     count)
;; => 471

;; part 2
(defn overlap?
  [[[s1 e1] [s2 e2]]]
  (or (is-inclusive? [[s1 e1] [s2 e2]])
      (and (<= s1 s2) (>= e1 s2))
      (and (>= s1 s2 )(>= e2 s1))))

(->> (slurp "04_input.txt")
     str/split-lines
     (map parse-line)
     (filter overlap?)
     count)
;; => 888
