(ns fox-goose-bag-of-corn.puzzle.approach.queue-solution
  (:require
    [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
    [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
    [fox-goose-bag-of-corn.puzzle.logic :as logically]
    [clojure.spec.alpha :as spec]
    [clojure.pprint :as pprint])
  (:import (clojure.lang PersistentQueue IPending)))

(defn queue? [e]
  (instance? PersistentQueue e))
  ;(instance? Double e))

(defn add-to-q [q items path-steps]
  (apply conj q
    (map
      #(conj path-steps %)
      items)))

(spec/fdef add-to-q
  :args (spec/cat
          :q queue?
          :items common-specs/step-instance-collection
          :path-steps common-specs/step-instance-collection-set)
  :ret queue?)


;; maintain a queue of path steps
;;   in each iteration:
;;     check to see if the last in popped is the answer
;;       if so:
;;          return desired path-steps just popped
;;          or add new possibilities to queue and try again

(defn river-crossing-plan [sp]
  (loop [simple-q (conj PersistentQueue/EMPTY sp)]
    (let [path-steps (spec/assert common-specs/step-instance-collection (first simple-q))
          all-possible-nexts (steps/every-possible-next-step path-steps)]
      (if (logically/result-found? (last path-steps))
        path-steps
        (recur
          (add-to-q (pop simple-q) all-possible-nexts path-steps))))))

(defn -main [& args]
  (time
    (pprint/pprint
      (river-crossing-plan
        [[#{:fox :goose :corn :you} #{:boat} #{}]]))))
