(ns wonderland-number.finder
  (:require [clojure.string :as string]))

(defn is-wonderland-number?
  [n]
  (let [products (for [factor (range 2 7)
                       :let [product (* n factor)
                             product-digits (set (str product))]]
                   product-digits)
        found? (= 1 (count (set products)))]
    (when found? n)))

(defn wonderland-number
  []
  (time (some (fn [n]
                (when-let [number-found (is-wonderland-number? n)]
                  (println "Found:" number-found)
                  number-found))
              (range 100000 1000000))))
