(ns fox-goose-bag-of-corn.puzzle.benchmarks
  (:require [fox-goose-bag-of-corn.puzzle :as puzzle]
            [fox-goose-bag-of-corn.puzzle.approach.queue-solution]
            [fox-goose-bag-of-corn.puzzle.approach.go-solution]
            [fox-goose-bag-of-corn.puzzle.approach.java-solution]
            [fox-goose-bag-of-corn.puzzle.approach.teachers-answer]
            [clojure.pprint :as pprint]
            [clojure.spec.alpha :as spec]
            [orchestra.spec.test :as spec-test]))


;
(def template (sorted-map
                ::teachers-answer fox-goose-bag-of-corn.puzzle.approach.teachers-answer/river-crossing-plan
                ::java fox-goose-bag-of-corn.puzzle.approach.java-solution/river-crossing-plan
                ::queue fox-goose-bag-of-corn.puzzle.approach.queue-solution/river-crossing-plan))
               ;::go fox-goose-bag-of-corn.fox-goose-bag-of-corn.puzzle.specs.puzzlee.approach.go-solution/river-crossing-plan})

;(defn run-result [plan-fn]
;  (future
;    (and
;      (try
;        (do
;          (fox-goose-bag-of-corn.puzzle.specs.puzzlee/with-plan plan-fn
;            (fox-goose-bag-of-corn.puzzle.specs.puzzlee/river-crossing-plan))
;          (System/currentTimeMillis))
;        (catch Throwable e :fail)))))

(defn run-result [plan-fn]
  (future
    (and
      (do
        (puzzle/with-plan plan-fn
          (puzzle/river-crossing-plan))
        (System/currentTimeMillis)))))

(spec/fdef run-result
           :args (spec/cat :plan-fn fn?)
           :ret future?)

(defn benchmarks-run []
  (reduce-kv
    (fn [m k v]
      (assoc m k (run-result (template k))))
    {}
    template))

(spec/fdef benchmarks-run
           :ret (spec/map-of keyword? future?))



(defn ran-benchmarks-run
  ([]
   (ran-benchmarks-run (benchmarks-run)))
  ([r]
   (reduce-kv
    (fn [m k v]
      (assoc m k (deref v)))
    {}
    r)))

(spec/fdef ran-benchmarks-run
           :ret (spec/map-of keyword? (spec/or :n number? :k keyword?)))

(defn ran-benchmarks-run-sorted
  ([]
   (ran-benchmarks-run-sorted (ran-benchmarks-run)))
  ([r]
   (->> r seq (sort-by
                (fn [e]
                  (cond
                    (number? (second e))
                    (-> e second (* -1))
                    :default
                    -1))))))

(defn bm-item? [e]
  (and
    (vector? e)
    (keyword? (first e))))
    ;(number? (second e))))

(spec/def ::bm-item bm-item?)

(spec/fdef ran-benchmarks-run-sorted
           :ret (spec/coll-of ::bm-item))

(def now-time (atom nil))

(defn time-nice [e]
  (str
    (- (last e) (deref now-time))
    " "
    "ms"))

(defn time-diff [e1 e2]
  (str "+"
    (apply -
      (map last [e1 e2]))))
;
(defn ran-benchmarks-run-decorated
  ([]
   (do
     (reset! now-time (System/currentTimeMillis))
     (ran-benchmarks-run-decorated (ran-benchmarks-run-sorted))))
  ([r]
   (map
     (fn [i e]
       (let [index (inc i)]
         (if (zero? i)
           (->> e
             ;(into [index])
             (into [index (time-nice e)]))
           ;else
           (->> e
             (into [index (time-diff (first r) e)])))))
     (range (count r))
     r)))

(spec-test/instrument)

(defn -main []
  (do
    ;(println
    (pprint/pprint
      (ran-benchmarks-run-decorated))
    (shutdown-agents)))
