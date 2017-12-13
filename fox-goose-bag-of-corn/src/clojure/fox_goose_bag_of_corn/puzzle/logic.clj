(ns fox-goose-bag-of-corn.puzzle.logic
  (:require [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
            [clojure.set]
            [fox-goose-bag-of-corn.puzzle.specs :as common-specs]))


(defn find-where-are-you? [prev-steps]
  (let [most-recent (last prev-steps)]
    (cond
      (:you (first most-recent))
      :left-bank
      (:you (last most-recent))
      :right-bank
      :else ; must be in the boat
      (let [second-most-recent (last (pop prev-steps))]
        (cond
          (:you (first second-most-recent))
          :boat-from-left
          (:you (last second-most-recent))
          :boat-from-right)))))


(spec/fdef find-where-are-you?
           :args (spec/cat :prev-steps (spec/coll-of common-specs/step-instance-set))
           :ret #{:left-bank, :right-bank, :boat-from-left, :boat-from-right})


(defn everyones-safe? [fgbc-step]
  (->>
    fgbc-step
    (filter                  ; only look at what doesn't have "you"
      #(not (:you %)))
    (filter                  ; ... and does contain an unfaithful pair
      #(or
         (clojure.set/subset?
           #{:fox :goose}
           %)
         (clojure.set/subset?
           #{:goose :corn}
           %)))
    empty?))                ; not empty = someone's not safe

(spec/fdef everyones-safe?
           :args (spec/cat :fgbc-step common-specs/step-instance-set))

(defn boat-capacity-respected? [fgbc-step]
  (->
    fgbc-step
    second
    count
    (<= 3)))

(spec/fdef boat-capacity-respected?
           :args (spec/cat :fgbc-step common-specs/step-instance-set))

(defn result-found? [fgbc-step]
;#(= ((.rootVal %) (index :right-bank))
  (-> fgbc-step
    (get (index :right-bank))
    (= #{:fox :goose :corn :you})))

(spec/fdef result-found?
           :args (spec/cat :fgbc-step common-specs/step-instance-set))
