(ns fox-goose-bag-of-corn.puzzle.approach.go-solution
  (:require [clojure.core.async :as async]
            [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
            [fox-goose-bag-of-corn.puzzle.logic :as logically]))

(def new-node-chan (async/chan)) ; this feels really dangerous.

(defn add-new-node!! [n]
  (async/>!! new-node-chan n))


(defn go-form [p]
  (async/go-loop []
    (let [path-steps
          (async/<! new-node-chan)
          all-possible-nexts (steps/every-possible-next-step path-steps)]
      (do
        (doseq [next all-possible-nexts]
          (let [farthest-so-far (conj path-steps next)]
            (if (logically/result-found? next)
              (deliver p farthest-so-far)
              ;else
              ;(async/>! new-node-chan farthest-so-far))))
              (async/put! new-node-chan farthest-so-far))))
        (recur)))))

(spec/fdef add-new-node!! :args (spec/cat :n (spec/coll-of common-specs/step-instance-set)))

(defn river-crossing-plan [sp]
  (let [simple-p (promise)]
    (do
      (doseq [_ (range 4)]
        (go-form simple-p))
      (add-new-node!! sp)
      (deref simple-p))))



