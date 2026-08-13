(ns tiny-maze.solver
  (:require [clojure.pprint :as pprint]
            [clojure.set :refer [difference]])
  (:import (clojure.lang PersistentQueue)))

(def x :x)
(def start :S)
(def end :E)

(defmulti next-maze-steps
  (fn [{:keys [maze position]}]
    (cond (= start (get-in maze (reverse position))) ::start
          (= end (get-in maze (reverse position)))   ::end)))

(defmethod next-maze-steps ::start
  [maze-step]
  [(update maze-step :maze assoc-in [0 0] x)])

(defmethod next-maze-steps ::end
  [{:keys [position] :as maze-step}]
  [(-> maze-step
       (update :maze assoc-in (reverse position) x)
       (assoc :last-update ::end))])

(defmethod next-maze-steps :default
  [{:keys [maze position maze-size] :as maze-step}]
  (let [directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (for [direction directions
          :let      [position'    [(+ (first position) (first direction))
                                   (+ (second position) (second direction))]
                     should-mark? (= 0 (get-in maze (reverse position')))
                     at-end?      (= end (get-in maze (reverse position')))]
          :when     (and (< (first position') (first maze-size))
                         (< (second position') (second maze-size))
                         (or should-mark? at-end?))]
      (-> maze-step
          (update :maze
                  (if at-end? identity #(assoc-in % (reverse position') x)))
          (assoc :position position')))))

(defn search
  [maze]
  (loop [queue (into PersistentQueue/EMPTY
                     [[{:position  [0 0]
                        :maze      maze
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
