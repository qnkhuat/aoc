(defn fifo-four-el
  [current-stack new-char]
  (if (< (count current-stack ) 4)
    (conj current-stack new-char)
    (conj (into [] (drop 1 current-stack)) new-char)))

(reduce (fn [[current-stack cnt] new-char]
          (let [new-stack (fifo-stack-four current-stack new-char)]
            (if (= (count (into #{} new-stack)) 4)
              (reduced [new-stack (inc cnt)])
              [new-stack (inc cnt)])))
        [[] 0]
        (apply list (slurp "input.txt")))
;; => [[\j \q \v \g] 1794]

(defn fifo-fourteen-el
  [current-stack new-char]
  (if (< (count current-stack ) 14)
    (conj current-stack new-char)
    (conj (into [] (drop 1 current-stack)) new-char)))

(reduce (fn [[current-stack cnt] new-char]
          (let [new-stack (fifo-fourteen-el current-stack new-char)]
            (if (= (count (into #{} new-stack)) 14)
              (reduced [new-stack (inc cnt)])
              [new-stack (inc cnt)])))
        [[] 0]
        (apply list (slurp "input.txt")))
;; => [[\p \r \h \v \l \g \f \t \c \b \n \w \j \q] 2851]
