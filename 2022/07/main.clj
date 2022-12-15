(require '[clojure.string :as str])

(def input (str/split-lines (slurp "input.txt")))

(defn cd
  [pwd dir]
  (if (= dir "..")
    (pop pwd)
    (conj pwd dir)))


(defn process-command
  [[tree pwd] command]
  (let [splits (str/split command #" ")]
    (case (nth splits 1)
      "ls" [tree pwd]
      "cd" [tree (cd pwd (nth splits 2))])))

(defn process-ls-output
  [[tree pwd] command]
  (let [[dir-or-size fname] (str/split command #" ")
        new-tree            (case dir-or-size
                              ;; new dir
                              "dir" (update-in tree (conj pwd fname) merge {})
                              (update-in tree pwd merge {fname (Integer/parseInt dir-or-size)}))]
    [new-tree pwd]))


(defn build-dir-tree
  [[tree pwd] command]
  (if (str/starts-with? command "$")
    (process-command [tree pwd] command)
    (process-ls-output [tree pwd] command)))

(def dir-tree
  (first (reduce build-dir-tree
                 [{} []] input)))


(defn has-sub-dir?
  [node]
  (and (map? node) (boolean (some (fn [[_k v]] (map? v)) node))))

(defn find-dirs
  [tree]
  (keys (filter (fn [[_k v]] (map? v)) tree)))

(defn find-dir-depth-first
  "f is a fn takes 2 thing: the whole tree and pwd "
  [tree f! pwd]
  (if (has-sub-dir? (get-in @tree pwd))
    (do (doall (map (fn [k] (find-dir-depth-first tree f! (conj pwd k))) (find-dirs (get-in @tree pwd))))
        (f! tree pwd))
    (f! tree pwd)))

(defn cal-size
  [node]
  (reduce (fn [agg [_k v]]
            (if (dir? v)
              (+ agg (:_size v))
              (+ agg v)))
          0 node))

(defn cal-dir-size!
  [tree-atom pwd]
  (swap! tree-atom assoc-in  (conj pwd :_size) (cal-size (get-in @tree-atom pwd))))


(defn flatten-dirs-size
  [tree]
  (let [dir-stats (atom {})]
    (find-dir-depth-first (atom tree) (fn [tree pwd]
                                        (swap! dir-stats merge {(str/join pwd) (get-in @tree (conj pwd :_size))})) [])))


(defn sum-size
  [dir-stats]
  (reduce (fn [agg [_pwd size]]
            (+ agg size)) 0 dir-stats))

(def tree-atom (atom dir-tree))
(find-dir-depth-first tree-atom cal-dir-size! [])

;; part 1
(->> (flatten-dirs-size @tree-atom)
     (filter #(<= (second %) 100000))
     sum-size)
;; => 1845346

;; part 2
(->> (flatten-dirs-size @tree-atom)
     (filter #(>= (second %) 3629016))
     (sort-by second)
     first
     second)
;; => 3636703
