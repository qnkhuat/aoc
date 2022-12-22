(require '[clojure.string :as str])

(def input
  (->> (slurp "08_input.txt")
       str/split-lines
       (map (fn [row] (map #(Integer/parseInt %) (str/split row #""))))))

(defn two-d-array
  [nested-lists]
  (let [col-count (count (first nested-lists))
        row-count (count nested-lists)
        values    (apply concat nested-lists)
        get-value (fn [row col]
                    (when (and (< row row-count)
                               (< col col-count))
                      (nth values (+ col (* row row-count)))))]
    {:get-value get-value
     :get-row   (fn [row]
                  (->> values
                       (drop (* col-count row))
                       (take col-count)))
     :get-col   (fn [col]
                  (for [r (range 0 row-count)]
                    (get-value r col)))
     :row-count row-count
     :col-count col-count}))


(defn one-side-visible?
  [side height]
  (every? #(< % height) side))

#_(def input
    [[3 0 3 7 3]
     [2 5 5 1 2]
     [6 5 3 3 2]
     [3 3 5 4 9]
     [3 5 3 9 0]])

(defn visible?
  [row-index col-index row col height]
  (or
    ;; visible from the left
    (one-side-visible? (take col-index row) height)
    ;; vislbe from the right
    (one-side-visible? (drop (inc col-index) row) height)
    ;; visible from the top
    (one-side-visible? (take row-index col) height)
    ;; visible from bottom
    (one-side-visible? (drop (inc row-index) col) height)))

;; part 1
(let [{:keys [get-value get-row get-col row-count col-count]} (two-d-array input)
      visible-trees (atom (+ row-count row-count col-count col-count -4))]
  (doseq [r (range 1 (dec row-count))
          c (range 1 (dec col-count))
          :when (visible? r c (get-row r) (get-col c) (get-value r c))]
    (swap! visible-trees inc))
  @visible-trees)
;; it runs for a long time
;; => 1820

(defn viewing-distance
  [trees view-height]
  trees
  view-height
  (reduce (fn [cnt tree-height]
            (if (>= tree-height view-height)
              (reduced (inc cnt))
              (inc cnt)))
          0 trees))

(defn scenic-score
  [row-index col-index row col height]
  (apply *
         [;; from the left
          (viewing-distance (reverse (take col-index row)) height)
          ;; from the right
          (viewing-distance (drop (inc col-index) row) height)
          ;; from the top
          (viewing-distance (reverse (take row-index col)) height)
          ;; from bottom
          (viewing-distance (drop (inc row-index) col) height)]))

;; part 2
(let [{:keys [get-value get-row get-col row-count col-count]} (two-d-array input)]
 (apply max (for [r (range 0 row-count)
                  c (range 0 col-count)]
              (scenic-score r c (get-row r) (get-col c) (get-value r c)))))
;; => 385112
