(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(defn rows-sums [m]
  (map #(reduce + %) m))

(defn cols-sums [m]
  (map
    (fn [i]
      (reduce + (map #(nth % i) m)))
    (range (count m))))


(defn diagonals-sums [m]
  (let [
        lr ; diagnol positions, top left -> bottom right
        (mapv #(vector % %) (range (count m)))
        rl ; diagnol positions, top right -> bottom left
        (loop [acc [[0, (dec (count m))]]]
          (if (= (count lr) (count acc))
            acc
            ;else
            (let [[x, y] (last acc)]
              (recur (conj acc [(inc x) (dec y)])))))]
    (mapv
      (fn [ps]
        (reduce + (map #(get-in m %) ps)))
      [lr, rl])))


(defn is-magical? [s]
  (->
    ((juxt rows-sums, cols-sums, diagonals-sums) s)
    flatten
    set
    count
    (= 1)))


(defn list->sq [l]
  (->>
    l
    (partition 3)
    (mapv vec)))

(defn magic-square [values] ; brute force attempt
  (let [all-possible-vals (combo/permutations values)]
    (loop [values-set all-possible-vals]
      (let [sq (list->sq (first values-set))]
        (if (is-magical? sq)
          sq
          ;else
          (recur (rest values-set)))))))