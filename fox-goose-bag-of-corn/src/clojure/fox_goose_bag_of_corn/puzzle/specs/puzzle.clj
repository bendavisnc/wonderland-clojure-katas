(ns fox-goose-bag-of-corn.puzzle.specs.puzzle
  (:require [clojure.spec.alpha :as spec]))


;;
;;
;; fox-goose-bag-of-corn.puzzle.specs.puzzlee specific

(def step-instance-vec
  (spec/and
    vector?
    (spec/coll-of
      (spec/and
        vector?
        (spec/coll-of keyword?)))))

;;
;; eg
;;   [{:fox}, {:goose, :moose}, {}]
(def step-instance-set
  (spec/and
    vector?
    (spec/coll-of
      (spec/and
        set?
        (spec/coll-of keyword?)))))

(def step-instance-collection
  (spec/coll-of
    (spec/or :set step-instance-set :vec step-instance-vec)))

(def step-instance-collection-set
  (spec/coll-of
    step-instance-set))



