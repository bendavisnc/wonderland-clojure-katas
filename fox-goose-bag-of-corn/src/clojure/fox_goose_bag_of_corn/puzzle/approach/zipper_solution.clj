(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution
  (:require
    [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
    [fox-goose-bag-of-corn.puzzle.specs.puzzle :as puzzle-specs]
    [fox-goose-bag-of-corn.puzzle.specs.tree :as tree-specs]
    [clojure.spec.alpha :as spec]
    [clojure.zip :as z]
    [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
    [fox-goose-bag-of-corn.puzzle.logic :as logically]
    [clojure.pprint :as pprint]
    [clojure.data.zip :as z-util]
    [orchestra.spec.test :as spec-test]))



(defn bottom-branch? [n]
  (not (z/down n)))

(spec/fdef bottom-branch?
           :args (spec/cat :n tree-specs/tree)
           :ret boolean?)

(defn get-lowest-branches [tree]
  (loop [acc []
         n tree]
    (cond
      (z/end? n)
      acc
      (bottom-branch? n)
      (recur (conj acc n) (z/next n))
      :default
      (recur acc (z/next n)))))

(spec/fdef get-lowest-branches
           :args (spec/cat :tree tree-specs/tree)
           :ret (spec/coll-of tree-specs/tree))

(defn branch->prev-steps [tree]
  (loop [acc () t* tree]
    (cond
      (nil? (z/up t*))
      (into [] (conj acc (:node-val (z/node t*))))
      :default
      (recur
        (conj acc (:node-val (z/node t*)))
        (z/up t*)))))


(spec/fdef branch->prev-steps
           :args (spec/cat :tree tree-specs/tree)
           :ret puzzle-specs/step-instance-collection-set)


(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      (fn [n]
        (logically/result-found? (:node-val (z/node n)))))
    (map
      #(branch->prev-steps %))
    first))


(spec/fdef found-result
           :args (spec/cat :tree tree-specs/tree)
           :ret (spec/or
                  :nil nil?
                  :result puzzle-specs/step-instance-collection-set))

(defn add-branches [tree next-steps]
  (if (empty? next-steps)
    tree
    (recur
      (z/insert-child tree (first next-steps))
      (rest next-steps))))

(spec/fdef add-branches
           ;:args (spec/cat :tree tree-specs/tree :next-steps puzzle-specs/step-instance-collection-set)
           :args (spec/cat :tree tree-specs/tree :next-steps coll?)
           :ret tree-specs/tree)


(defn fn-replace [n]
  {:node-val
    #(add-branches n (steps/every-possible-next-step (branch->prev-steps n)))})

(defn expand-tree-phase1 [tree]
  (loop [n tree]
    (cond
      (z/end? n)
      (mapzipper (z/root n))
      (bottom-branch? n)
      (recur
        (-> n
          (z/replace (fn-replace n))
          z/next))
      :default
      (recur (z/next n)))))

(spec/fdef expand-tree-phase1
           :args (spec/cat :tree tree-specs/tree)
           :ret tree-specs/tree)

(defn expand-tree-phase2 [tree]
  (loop [n tree]
    (cond
      (z/end? n)
      (mapzipper (z/root n))
      (fn? (:node-val (z/node n)))
      (recur
        (z/replace
          n
          (z/node ((:node-val (z/node n))))))
      :default
      (recur (z/next n)))))

(spec/fdef expand-tree-phase2
           :args (spec/cat :tree tree-specs/tree)
           :ret tree-specs/tree)

(defn build-up-tree [tree]
  (expand-tree-phase2
    (expand-tree-phase1 tree)))

(spec/fdef build-up-tree
           :args (spec/cat :tree tree-specs/tree)
           :ret tree-specs/tree)

(defn river-crossing-plan [sp]
  (loop [simple-t (mapzipper {:node-val sp})]
    (or
      (found-result simple-t)
      (recur
        (build-up-tree simple-t)))))

(spec-test/instrument)

(defn -main [& args]
  (time
    (pprint/pprint
      (river-crossing-plan
        [#{:fox :goose :corn :you} #{:boat} #{}]))))
