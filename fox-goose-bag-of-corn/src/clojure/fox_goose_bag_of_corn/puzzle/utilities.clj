(ns fox-goose-bag-of-corn.puzzle.utilities
  (:require [clojure.zip :as z]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.specs.tree :as tree-specs]))

(def index {:left-bank 0, :boat 1, :right-bank 2})


(defn mapzipper [m]
  (z/zipper
    ; branch
    (fn [b]
      (spec/valid? tree-specs/tree-node b))
      ;(and
      ;  b
      ;  (map? b)
      ;  (:node-val b))
    ; children (unzip)
    (fn [n]
      (:children n))
    ; make node (zip)
    (fn [n c]
      (assoc n :children (vec c)))
    m))


(defn root
  [loc]
  (if (= :end (loc 1))
    loc
    (let [p (z/up loc)]
      (if p
        (recur p)
        loc))))
