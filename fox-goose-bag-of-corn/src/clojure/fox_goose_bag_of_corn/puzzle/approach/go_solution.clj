(ns fox-goose-bag-of-corn.puzzle.approach.go-solution
  (:require [clojure.core.async :as async]
            [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
            [fox-goose-bag-of-corn.puzzle.logic :as logically]))

(def production-chan (async/chan 1000))
(def evaluation-chan (async/chan 1000))

(defn add-first-node!! [n]
  (async/>!! production-chan n))

(defn go-find-form [p]
  (async/go-loop []
    (let [path-steps (async/<! evaluation-chan)]
      (if (logically/result-found? (last path-steps))
        (do
          (async/close! production-chan)
          (async/close! evaluation-chan)
          (deliver p path-steps))
        ;else
        (recur)))))


(defn go-put-form []
  (async/go-loop []
    (let [path-steps
          (async/<! production-chan)
          all-possible-nexts (steps/every-possible-next-step path-steps)]
      (do
        (doseq [next all-possible-nexts]
          (let [farthest-so-far (conj path-steps next)]
            (async/put! production-chan farthest-so-far)
            (async/>! evaluation-chan farthest-so-far)))
        (recur)))))


(defn go-form [p]
  (do
    (go-find-form p)
    (go-put-form)))

(spec/fdef add-first-node!! :args (spec/cat :n (spec/coll-of common-specs/step-instance-set)))

(defn river-crossing-plan [sp]
  (let [simple-p (promise)]
    (do
      (go-form simple-p)
      (add-first-node!! sp)
      (deref simple-p))))


