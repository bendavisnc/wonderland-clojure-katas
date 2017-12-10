(ns fox-goose-bag-of-corn.puzzle.benchmarks
  (:require [fox-goose-bag-of-corn.puzzle :as puzzle]
            [fox-goose-bag-of-corn.puzzle.approach.queue-solution]
            [fox-goose-bag-of-corn.puzzle.approach.go-solution]
            [fox-goose-bag-of-corn.puzzle.approach.java-solution]
            [clojure.pprint :as pprint]
            [clojure.spec.alpha :as spec]
            [orchestra.spec.test :as spec-test]))


;
(def template {::java fox-goose-bag-of-corn.puzzle.approach.java-solution/river-crossing-plan
               ::queue fox-goose-bag-of-corn.puzzle.approach.java-solution/river-crossing-plan})
               ;::go fox-goose-bag-of-corn.puzzle.approach.go-solution/river-crossing-plan})

(defn run-result [plan-fn]
  (future
    (and
      (puzzle/with-plan plan-fn
        (puzzle/river-crossing-plan))
      (System/currentTimeMillis))))


(defn benchmarks-run []
  (reduce-kv
    (fn [m k v]
      (assoc m k (run-result (template k))))
    {}
    template))

(defn ran-benchmarks-run
  ([]
   (ran-benchmarks-run (benchmarks-run)))
  ([r]
   (reduce-kv
    (fn [m k v]
      (assoc m k (deref v)))
    {}
    r)))

(defn ran-benchmarks-run-sorted
  ([]
   (ran-benchmarks-run-sorted (ran-benchmarks-run)))
  ([r]
   (->> r seq (sort-by #(-> % second (* -1))))))

(defn ran-benchmarks-run-decorated
  ([]
   ;(ran-benchmarks-run-sorted))
   (ran-benchmarks-run-decorated (ran-benchmarks-run-sorted)))
  ([r]
   r))
   ;(map
   ;   (fn [i, r-elem]
   ;     (println i)
   ;     (println r-elem)
   ;     (into [i] r-elem)
   ;   (count r)
   ;   r]))

(spec/fdef ran-benchmarks-run-decorated :args (spec/* seq?))

(spec-test/instrument)

(defn -main []
  (do
    (pprint/pprint
      ;(ran-benchmarks-run-sorted)
      (ran-benchmarks-run-decorated))
    (shutdown-agents)))
