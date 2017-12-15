;(ns fox-goose-bag-of-corn.puzzle.benchmarks
;  (:require [fox-goose-bag-of-corn.puzzle :as puzzle]
;            [fox-goose-bag-of-corn.puzzle.approach.queue-solution]
;            [fox-goose-bag-of-corn.puzzle.approach.go-solution]
;            [fox-goose-bag-of-corn.puzzle.approach.java-solution]
;            [fox-goose-bag-of-corn.puzzle.approach.teachers-answer]
;            [fox-goose-bag-of-corn.puzzle.approach.zipper-solution]
;            ;[criterium.core :as criterium]
;            [clojure.pprint :as pprint]
;            [clojure.spec.alpha :as spec]
;            [orchestra.spec.test :as spec-test]))
;
;(use 'criterium.core)
;;
;(def template (sorted-map
;                ::teachers-answer fox-goose-bag-of-corn.puzzle.approach.teachers-answer/river-crossing-plan
;                ::java fox-goose-bag-of-corn.puzzle.approach.java-solution/river-crossing-plan))
;                ;::queue fox-goose-bag-of-corn.puzzle.approach.queue-solution/river-crossing-plan))
;                ;::zipper fox-goose-bag-of-corn.puzzle.approach.zipper-solution/river-crossing-plan))
;               ;::go fox-goose-bag-of-corn.fox-goose-bag-of-corn.puzzle.specs.puzzlee.approach.go-solution/river-crossing-plan})
;
;
;(defn run-result [plan-fn]
;  (bench
;      (puzzle/with-plan plan-fn
;        (puzzle/river-crossing-plan))))
;
;(spec/fdef run-result
;           :args (spec/cat :plan-fn fn?))
;           ;:ret future?)
;
;(defn benchmarks-run []
;  (reduce-kv
;    (fn [m k v]
;      (assoc m k (run-result (template k))))
;    {}
;    template))
;
;(spec-test/instrument)
;
;(defn -main []
;  (do
;    (pprint/pprint
;      (benchmarks-run))
;      ;(ran-benchmarks-run-decorated))
;    (shutdown-agents)))
