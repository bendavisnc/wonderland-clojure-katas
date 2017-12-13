(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution
  (:require
    [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
    [clojure.spec.alpha :as spec]
    [clojure.zip :as z]
    [clojure.spec.test.alpha :as spec-test]
    [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
    [fox-goose-bag-of-corn.puzzle.logic :as logically]
    [clojure.pprint :as pprint]
    [clojure.walk :as walk]))


(defn bottom-branch? [n]
  (if (nil? n)
    nil
    (nil? (z/down n))))

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

(defn branch->prev-steps [tree]
  (loop [acc []
         n tree]
    (cond
      (nil? (z/up n))
      (conj acc (z/node n))
      :default
      (recur (conj acc (z/node n)) (z/up n)))))

(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      #(logically/result-found? (z/node %)))
    (map
      #(branch->prev-steps %))
    first))

(defn add-branches [tree next-steps]
  (if (empty? next-steps)
    tree
    (recur
      (z/insert-child tree (first next-steps))
      (rest next-steps))))

(defn tree-walker-fn [n]
  (cond
    (bottom-branch? n)
    (add-branches
      n
      (steps/every-possible-next-step (branch->prev-steps n)))
    :default
    n))
;(loop [current-node tree]

(defn expanded-tree [tree]
  (walk/postwalk
    tree-walker-fn
    tree))


(defn river-crossing-plan [sp]
  (loop [simple-t (z/vector-zip [sp])]
    (or
      (found-result simple-t)
      (recur
        (z/vector-zip
          (expanded-tree simple-t))))))

(defn -main [& args]
  (time
    (pprint/pprint
      (river-crossing-plan
        [[#{:fox :goose :corn :you} #{:boat} #{}]]))))
