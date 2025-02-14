(ns fox-goose-bag-of-corn.puzzle
  (:require [fox-goose-bag-of-corn.logic :as logic]
            [fox-goose-bag-of-corn.steps :as steps])
  (:import (clojure.lang PersistentQueue)))

(defn expand-paths
  "Expands the BFS queue by generating new possible paths while avoiding revisited states."
  [queue]
  (let [steps (first queue)
        next-steps (steps/every-possible-next-step steps)]
    (apply conj (pop queue) (map #(conj steps %) next-steps))))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn river-crossing-plan
  "Finds a valid sequence of steps using BFS without explicit recursion."
  []
  (->> (iterate expand-paths
                (conj PersistentQueue/EMPTY
                      (mapv (partial mapv set) start-pos)))
       (filter (comp logic/result-found? last peek)) ;; Check if the last
                                                     ;; step is the goal.
       first ;; Return the found queue's most recent sequence of steps.
       first))