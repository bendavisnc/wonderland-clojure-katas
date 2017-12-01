(ns fox-goose-bag-of-corn.puzzle.logic)

(defn find-where-are-you? [prev-steps]
  (let [most-recent (last prev-steps)]
    (cond
      (:you (first most-recent))
      :left-bank
      (:you (last most-recent))
      :right-bank
      :else ; must be in the boat
      (let [second-most-recent (last (pop prev-steps))]
        (cond
          (:you (first second-most-recent))
          :boat-from-left
          (:you (last second-most-recent))
          :boat-from-right)))))

(defn everyones-safe? [fgbc-step]
  (->>
    fgbc-step
    (filter                  ; only look at what doesn't have "you"
      #(not (:you %)))
    (filter                  ; ... and does contain an unfaithful pair
      #(or
         (clojure.set/subset?
           #{:fox :goose}
           %)
         (clojure.set/subset?
           #{:goose :corn}
           %)))
    empty?))                ; not empty = someone's not safe

(defn boat-capacity-respected? [fgbc-step]
  (->
    fgbc-step
    second
    count
    (<= 3)))
