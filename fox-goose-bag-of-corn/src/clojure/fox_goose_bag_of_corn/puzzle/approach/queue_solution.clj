(ns fox-goose-bag-of-corn.puzzle.approach.queue-solution
  (:require
    [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
    [clojure.spec.alpha :as spec]
    [clojure.spec.test.alpha :as spec-test]
    [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
    [fox-goose-bag-of-corn.puzzle.logic :as logically])
  (:import (clojure.lang PersistentQueue)))


(defn add-to-q [q items path-steps]
  (apply conj q
    (map
      #(conj path-steps %)
      items)))



(defn river-crossing-plan [sp]
  (loop [simple-q (conj PersistentQueue/EMPTY sp)]
    (let [path-steps (spec/assert common-specs/step-instance-collection (first simple-q))
          all-possible-nexts (steps/every-possible-next-step path-steps)]
      (if (logically/result-found? (last path-steps))
        path-steps
        (recur
          (add-to-q (pop simple-q) all-possible-nexts path-steps))))))

