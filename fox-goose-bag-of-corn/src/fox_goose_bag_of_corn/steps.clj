(ns fox-goose-bag-of-corn.steps
  (:require [clojure.set :as set]
            [fox-goose-bag-of-corn.logic :as logic]
            [fox-goose-bag-of-corn.helpers :refer [step-key->index]]))

(defn every-possible-next-from-*
  [from to]
  (fn [step]
    (let [items-to-carry (set/difference (get step (step-key->index from))
                                         #{:you :boat})
          every-possible-step
          (mapv (fn [carry-item]
                  (-> step
                      (update (step-key->index from)
                              #(set/difference % #{:you carry-item}))
                      (update (step-key->index to)
                              #(set/union
                                %
                                (if carry-item #{:you carry-item} #{:you})))))
                (conj items-to-carry nil))]
      (->> every-possible-step
           (filter logic/boat-capacity-respected?)
           (filter logic/everyones-safe?)))))

(def every-possible-next-from
  {:left-bank (every-possible-next-from-* :left-bank :boat)
   :boat-from-left (every-possible-next-from-* :boat :right-bank)
   :right-bank (every-possible-next-from-* :right-bank :boat)
   :boat-from-right (every-possible-next-from-* :boat :left-bank)})

(defn every-possible-next-step
  "Given a sequence of previous steps, returns a sequence of all possible next steps."
  [steps]
  (let [coming-from (logic/find-where-are-you? steps)
        most-recent (last steps)]
    ((every-possible-next-from coming-from) most-recent)))
