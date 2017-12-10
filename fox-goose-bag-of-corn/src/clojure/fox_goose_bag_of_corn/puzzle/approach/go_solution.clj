(ns fox-goose-bag-of-corn.puzzle.approach.go-solution
  (:require [clojure.core.async :as async]
            [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
            [fox-goose-bag-of-corn.puzzle.logic :as logically]
            [clojure.pprint :as pprint])
  (:import (clojure.lang IPending)))


(def origin-chan (async/chan 1))
(def tree-growing-chan (async/chan 1))
(def all-chans [origin-chan, tree-growing-chan])

(defn add-first-node!! [n]
  (async/>!! origin-chan n))

(spec/fdef add-first-node!! :args (spec/cat :n common-specs/step-instance-collection-set))

(defn close-and-deliver [p, path-steps]
  (do
    (doseq [c all-chans]
      (async/close! c))
    (deliver p path-steps)))
  ;else
(spec/fdef close-and-deliver :args (spec/cat :p #(instance? IPending %) :success-r common-specs/step-instance-collection-set))


(defn search-successful? [path-steps]
  (logically/result-found? (last path-steps)))

;;
;;
;; go evaluations

(defn are-we-done-evaluation [p]
  (async/go-loop []
    (let [path-steps (async/<! origin-chan)]
      (cond
        (search-successful? path-steps)
        (close-and-deliver p path-steps)
        :default
        (do
          (async/>! tree-growing-chan path-steps)
          (recur))))))

(defn tree-growing-process []
  (async/go-loop []
    (let [root-path-steps (async/<! tree-growing-chan)]
      (do
        ;(println "about to grow tree")
        (doseq [next (steps/every-possible-next-step root-path-steps)]
          (let [farthest-so-far (conj root-path-steps next)]
            ;(async/>! origin-chan farthest-so-far)
            (async/put! origin-chan farthest-so-far))) ; DEVNOTE = put! seems wrong here
        (recur)))))


(defn aggregate-go-evaluation [p]
  (do
    (doseq [_ (range 100)]
      (are-we-done-evaluation p))
    (tree-growing-process)))

(def go-evaluation aggregate-go-evaluation)


(defn river-crossing-plan [sp]
  (let [simple-p (promise)]
    (do
      (go-evaluation simple-p)
      (add-first-node!! sp)
      (deref simple-p))))


(defn -main [& args]
  (time
    (pprint/pprint
      (river-crossing-plan
        [[#{:fox :goose :corn :you} #{:boat} #{}]]))))

