(ns fox-goose-bag-of-corn.specs
  (:require [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.steps]
            [fox-goose-bag-of-corn.logic]
            [fox-goose-bag-of-corn.puzzle])
  (:import (clojure.lang IPersistentVector PersistentQueue)))

(def step-values #{:fox :goose :corn :you :boat})

(def step-partition (spec/coll-of step-values))

(def step (spec/coll-of step-partition :count 3 :kind vector?))

(def steps (spec/coll-of step))

(def steps-queue (spec/every steps :kind (partial instance? PersistentQueue)))

(spec/fdef fox-goose-bag-of-corn.steps/every-possible-next-step
 :args
 (spec/cat :steps
           steps))

(spec/fdef fox-goose-bag-of-corn.puzzle/expand-paths
 :args
 (spec/cat :queue
           ;steps-queue
           seq))


(spec/fdef fox-goose-bag-of-corn.logic/result-found?
 :args
 (spec/cat :step
           step))

(spec/fdef fox-goose-bag-of-corn.logic/find-where-are-you?
 :args
 (spec/cat :steps
           steps))

(spec/fdef fox-goose-bag-of-corn.logic/everyones-safe?
 :args
 (spec/cat :step
           step))

(spec/fdef fox-goose-bag-of-corn.puzzle/river-crossing-plan
 :args
 (spec/cat))

