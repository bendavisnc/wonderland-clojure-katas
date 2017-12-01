(ns fox-goose-bag-of-corn.puzzle.specs
  (:require [clojure.spec.alpha :as spec]))

(def step-instance-vec
  (spec/and
    vector?
    (spec/coll-of
      (spec/and
        vector?
        (spec/coll-of keyword?)))))




(def step-instance-set
  (spec/and
    vector?
    (spec/coll-of
      (spec/and
        set?
        (spec/coll-of keyword?)))))
