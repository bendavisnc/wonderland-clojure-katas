(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn rows-sums [m]
  (mapv #(apply + %) m))

(defn cols-sums [m]
  (apply mapv + m))

(defn diagonals-sums [m]
  (let [n (count m)
        lr (mapv #(get-in m [% %]) (range n))       ;; Top-left to bottom-right
        rl (mapv #(get-in m [% (- (dec n) %)]) (range n))] ;; Top-right to bottom-left
    [(apply + lr) (apply + rl)]))

(defn is-magical? [s]
  (->> ((juxt rows-sums cols-sums diagonals-sums) s)
       flatten
       set
       count
       (= 1)))

(defn list->sq [l]
  (->> l
       (partition 3)
       (mapv vec)))

(defn magic-square [values]  ;; Brute force attempt
  (some #(let [sq (list->sq %)]
           (when (is-magical? sq) sq))
        (combo/permutations values)))
