(ns fox-goose-bag-of-corn.puzzle.specs.tree
  (:require [clojure.spec.alpha :as spec]))






;;
;;
;; tree specific

;(defn- tree? [t])
  ;(spec/))
  ;(and
  ;  (double? t))
    ;(vector t)
    ;(= 2 (count t))
    ;(get-in t [0 :node-val]))


(defn tree-node? [b]
  (and
    (map? b)
    (:node-val b)))

(def tree-node tree-node?)

;(def tree tree?)

(def tree
  (spec/tuple
    tree-node?
    (spec/or
      :end #{:end}
      :nil nil?
      :map map?)))
