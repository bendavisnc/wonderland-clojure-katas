(ns tiny-maze.solver
  (:require [clojure.pprint :as pprint]
            [clojure.set :refer [difference]])
  (:import (clojure.lang PersistentQueue)))

(def x :x)
(def start :S)
(def end :E)

(defn add [a b] (mapv + a b))

(defn maze-get [maze position] (get-in maze (reverse position)))

(defn maze-set
  [maze position element]
  (assoc-in maze (reverse position) element))

(defn maze-position-within-bounds
  [& {:keys [maze-size position]}]
  (every? true? (map < position maze-size)))

;!zprint {:format :skip}
(defmulti next-maze-steps
  (fn [{:keys [maze position]}]
    (condp = (maze-get maze position)
      start ::start
      end   ::end
      x     :default)))

(defmethod next-maze-steps ::start
  [maze-step]
  [(update maze-step :maze maze-set [0 0] x)])

(defmethod next-maze-steps ::end
  [{:keys [position], :as maze-step}]
  [(-> maze-step
       (update :maze maze-set position x)
       (assoc :last-update ::end))])

(defmethod next-maze-steps :default
  [{:keys [maze position maze-size], :as maze-step}]
  (flatten (let [directions [[-1 0] [1 0] [0 -1] [0 1]]]
             (for [direction directions
                   :let      [position' (add position direction)]
                   :when     (maze-position-within-bounds :maze-size maze-size
                                                          :position  position')
                   :let      [maze-step' (assoc maze-step :position position')]]
               (condp = (maze-get maze position')
                 end [maze-step']
                 0   [(update maze-step' :maze maze-set position' x)]
                 [])))))

(defn search
  [maze]
  (loop [queue (into PersistentQueue/EMPTY
                     [[{:position  [0 0],
                        :maze      maze,
                        :maze-size [(count (first maze)) (count maze)]}]])]
    (let [maze-steps       (peek queue)
          _ (assert vector? maze-steps)
          _ (assert (seq maze-steps))
          maze-step-latest (last maze-steps)
          _ (assert (= (set [:position :maze :maze-size])
                       (difference (set (keys maze-step-latest))
                                   #{:last-update}))
                    (format "Unexpected `maze-step`, %s." maze-step-latest))]
      (if (= ::end (:last-update maze-step-latest))
        maze-steps
        (recur (into (pop queue)
                     (map (partial conj maze-steps)
                          (next-maze-steps maze-step-latest))))))))


(defn solve-maze
  [maze]
  (let [search-result  (search maze)
        _ (assert (seq search-result))
        {:keys [maze]} (last search-result)]
    maze))
