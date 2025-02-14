(ns fox-goose-bag-of-corn.logic
  (:require [clojure.set :as set]
            [fox-goose-bag-of-corn.helpers :refer [step-key->index]]))

(defn find-where-are-you?
  "Determines the current location of `:you` based on the most recent step.
   - If `:you` is on the first or last position, return `:left-bank` or `:right-bank`.
   - Otherwise, infer if `:you` came from `:boat-from-left` or `:boat-from-right`
     based on the second-most-recent step."
  [prev-steps]
  (let [most-recent (last prev-steps)]
    (or (some (fn [[k v]] (when (:you v) k))
              {:left-bank (first most-recent) :right-bank (last most-recent)})
        (let [second-most-recent (nth prev-steps (- (count prev-steps) 2) nil)]
          (some (fn [[k v]] (when (:you v) k))
                {:boat-from-left (first second-most-recent)
                 :boat-from-right (last second-most-recent)})))))

(defn everyones-safe?
  "Checks whether any unfaithful pair (fox/goose, goose/corn) is left alone without `:you`."
  [step]
  (not (some #(or (set/subset? #{:fox :goose} %)
                  (set/subset? #{:goose :corn} %))
             (remove #(contains? % :you) step))))

(defn boat-capacity-respected?
  "Ensures that the boat does not exceed its capacity of 3 items."
  [step]
  (<= (count (get step (step-key->index :boat))) 3))

(defn result-found?
  "Checks if all characters (fox, goose, corn, and you) are safely on the right bank."
  [step]
  (= #{:fox :goose :corn :you} (get step (step-key->index :right-bank))))
