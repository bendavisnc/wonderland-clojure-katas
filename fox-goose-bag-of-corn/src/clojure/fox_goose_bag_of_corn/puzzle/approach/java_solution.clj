(ns fox-goose-bag-of-corn.puzzle.approach.java-solution
  (:require [fox-goose-bag-of-corn.puzzle.logic :as logically]
            [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
            [fox-goose-bag-of-corn.puzzle.step-generation :as steps])

  (:import (fox_goose_bag_of_corn.java TreeNode)))


(defn tree-with-root [r]
  (new TreeNode r))

(defn get-lowest-branches [^TreeNode tree]
  (.getLowestBranches tree))

(defn add-branches [^TreeNode tree, new-branch-vals]
  (.addBranches tree new-branch-vals))

(defn branch->prev-steps [^TreeNode branch]
  (vec
    (.nodeToValsList branch)))



(defn build-up-tree [tree]
  (doseq [branch (get-lowest-branches tree)]
    (add-branches
      branch
      (steps/every-possible-next-step
      ;(builder-fn
        (branch->prev-steps branch)))))

(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      #(= ((.rootVal %) (index :right-bank)) #{:fox :goose :corn :you}))
    (map
      #(branch->prev-steps %))
    first))


;;
;; A recursive approach that on each call:
;; 1. Adds new branches to its search tree.
;; 2. Looks for a result and either:
;;      on success returns that result
;;      or on failure continues recursively
(defn river-crossing-plan [sp]
  (let [tree (tree-with-root (first sp))]
    (loop []
      (do
        (build-up-tree tree)
        (or
          (found-result tree)
          (recur))))))
