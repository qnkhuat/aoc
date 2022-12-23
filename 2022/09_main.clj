(require '[clojure.string :as str]
         '[clojure.set :as set])

(def steps
  (->> (slurp "09_input.txt")
       str/split-lines
       (map #(str/split % #" "))
       (map #(update % 0 (comp keyword str/lower-case)))
       (map #(update % 1 parse-long))))

(defn move
  ([pos direction]
   (move pos direction 1))
  ([[x y :as pos] direction step]
   (case direction
     :u  [x (+ y step)]
     :ur (-> (move pos :u step)
             (move :r step))
     :ul (-> (move pos :u step)
             (move :l step))
     :d  [x (- y step)]
     :dr (-> (move pos :d step)
             (move :r step))
     :dl (-> (move pos :d step)
             (move :l step))
     :r  [(+ x step) y]
     :l  [(- x step) y])))

(defn touching?
  [[x1 y1] [x2 y2]]
  (every? (fn [[a b]] (<= (abs (- a b)) 1))
          [[x1 x2] [y1 y2]]))

(defn maybe-move-tail
  [[x-t y-t :as tail-pos] [x-h y-h :as head-pos]]
  (if (touching? tail-pos head-pos)
    tail-pos
    (cond
      (= x-h x-t)
      (if (> y-h y-t)
        (move tail-pos :u)
        (move tail-pos :d))

      (= y-h y-t)
      (if (> x-h x-t)
        (move tail-pos :r)
        (move tail-pos :l))

      :else
      (if (> y-h y-t)
        ;; move up
        (if (> x-h x-t)
          (move tail-pos :ur)
          (move tail-pos :ul))
        (if (> x-h x-t)
          (move tail-pos :dr)
          (move tail-pos :dl))))))

(defn move-til-touch
  [tail head]
  (loop [tail            tail
         head            head
         tail-pos-walked #{}]
    (if (touching? tail head)
      [tail head tail-pos-walked]
      (let [new-tail-pos (maybe-move-tail tail head)]
        (recur new-tail-pos head (conj tail-pos-walked new-tail-pos))))))

;; part 1
(def result (reduce
              (fn [{:keys [tail head tail-pos-walked]} [direction step :as _step]]
                (let [[tail head tail-walked]
                      (move-til-touch tail (move head direction step))]
                  {:tail tail
                   :head head
                   :tail-pos-walked (set/union tail-pos-walked tail-walked)}))
              {:tail [0 0]
               :head [0 0]
               :tail-pos-walked #{[0 0]}}
              steps))
(-> result
    :tail-pos-walked
    count)
;; => 5513

;; part 2

(defn move-tail-to-meet-head
  [tail head]
  (reduce
    (fn [{:keys [tail head tail-pos-walked]} [direction step :as _step]]
      (let [[tail head tail-walked]
            (move-til-touch tail (move head direction step))]
        {:tail tail
         :head head
         :tail-pos-walked (set/union tail-pos-walked tail-walked)}))
    {:tail            tail
     :head            head
     :tail-pos-walked #{[0 0]}}
    steps))

;; part 1
(def result-2
  (reduce
    (fn [{:keys [rope tail-pos-walked]} [direction step :as _step]]
      (let [[head & tails] rope
            new-head (move head direction step)
            new-rope (concat [new-head] tails)
            pairs    (partition 2 1 new-rope)]
        (reduce (fn [{:keys [rope]} [head tail]]
                  (let [[tail _head sub-tail-pos-walked] (move-til-touch tail head)]
                    {:rope            (conj rope tail)
                     ;; just do a replacement because we only care about the last anyway
                     :tail-pos-walked (set/union tail-pos-walked sub-tail-pos-walked)}))
                {:rope            [new-head]
                 :tail-pos-walked #{}}
                pairs)))
    {:rope            (repeat 10 [0 0]) ;; head is first
     :tail-pos-walked #{[0 0]}}
    steps))

(-> result-2
    :tail-pos-walked
    count)
;; => 2364

;; right answer 2427
