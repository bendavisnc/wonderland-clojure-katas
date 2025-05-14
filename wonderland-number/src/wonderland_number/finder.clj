(ns wonderland-number.finder
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn is-wonderland-number?
  [digits]
  (let [n (Long/valueOf (string/join digits))
        products (for [factor (range 2 7)
                       :let [product (* n factor)
                             product-padded (format "%07d" product)
                             product-digits (set (map #(Character/digit % 10)
                                                      product-padded))]]
                   product-digits)
        found? (= 1 (count (set products)))]
    (when found? n)))

(defn wonderland-number
  []
  (some (fn [digits]
          (when-let [number-found (is-wonderland-number? digits)] number-found))
        (combo/permuted-combinations (range 0 10) 6)))
