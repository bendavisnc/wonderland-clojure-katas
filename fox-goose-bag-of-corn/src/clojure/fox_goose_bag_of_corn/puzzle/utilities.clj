(ns fox-goose-bag-of-corn.puzzle.utilities
  (:require [clojure.zip :as z]))

(def index {:left-bank 0, :boat 1, :right-bank 2})


(defn mapzipper [m]
  (z/zipper
    ; branch
    (fn [b]
      (and
        b
        (map? b)
        (:node-val b)))
    ; children (unzip)
    (fn [n]
      (:children n))
    ; make node (zip)
    (fn [n c]
      (assoc n :children c))
    m))
