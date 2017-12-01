(ns fox-goose-bag-of-corn.puzzle.java-solution
  (:require [fox-goose-bag-of-corn.puzzle.logic :as logically])
  (:import (fox_goose_bag_of_corn.java TreeNode)))

(def index {:left-bank 0, :boat 1, :right-bank 2})

(defn tree-with-root [r]
  (new TreeNode r))

(defn get-lowest-branches [^TreeNode tree]
  (.getLowestBranches tree))

(defn add-branches [^TreeNode tree, new-branch-vals]
  (.addBranches tree new-branch-vals))

(defn branch->prev-steps [^TreeNode branch]
  (vec
    (.nodeToValsList branch)))


(defn every-possible-next-from-* [from, to]
  (fn [fgbc-step]
    (let [
          items-to-carry
          (clojure.set/difference (fgbc-step (index from)) #{:you :boat})
          every-possible-step
          (mapv
            (fn [carry-item]
              (->
                fgbc-step
                (update (index from) #(clojure.set/difference % #{:you carry-item}))
                (update (index to) #(clojure.set/union % (if carry-item #{:you carry-item} #{:you})))))
            (conj items-to-carry nil))]
      (->>
        every-possible-step
        (filter logically/boat-capacity-respected?)
        (filter logically/everyones-safe?)))))

(def every-possible-next-from
  {:left-bank (every-possible-next-from-* :left-bank :boat)
   :boat-from-left (every-possible-next-from-* :boat :right-bank)
   :right-bank (every-possible-next-from-* :right-bank :boat)
   :boat-from-right (every-possible-next-from-* :boat :left-bank)})

(defn every-possible-next-step [prev-steps]
  (let [coming-from (logically/find-where-are-you? prev-steps),
        most-recent (last prev-steps)]
    ((every-possible-next-from coming-from) most-recent)))

(defn build-up-tree [tree]
  (doseq [branch (get-lowest-branches tree)]
    (add-branches
      branch
      (every-possible-next-step
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
;;      on success returns that result))
;;        or on failure continues recursively))
(defn river-crossing-plan [sp]
  (let [tree (tree-with-root (first sp))]
    (loop []
      (do
        (build-up-tree tree)
        (or
          (found-result tree)
          (recur))))))
