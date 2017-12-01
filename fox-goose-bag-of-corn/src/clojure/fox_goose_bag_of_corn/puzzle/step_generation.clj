(ns fox-goose-bag-of-corn.puzzle.step-generation
  (:require [fox-goose-bag-of-corn.puzzle.logic :as logically]
            [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
            [clojure.spec.test.alpha :as spec-test]))

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

(spec/fdef every-possible-next-step
           :args (spec/cat :prev-steps (spec/coll-of common-specs/step-instance-set))
           :ret (spec/coll-of common-specs/step-instance-set))

(spec-test/instrument `every-possible-next-step)
