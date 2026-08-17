(ns alphabet-cipher.coder
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]))

(defn alphabet-cipher-chart-next-row
  [row]
  (vec (concat (rest row) [(first row)])))

(defn char->index [c] (- (int c) (int \a)))

(def alphabet-cipher-chart
  (let [alphabet-chars (vec (map char (range (int \a) (inc (int \z)))))
        character-grid (vec (take (count alphabet-chars)
                                  (iterate alphabet-cipher-chart-next-row
                                           alphabet-chars)))]
    (reduce (fn [acc k]
              (let [[column-char row-char] k]
                (assoc-in acc
                 [column-char row-char]
                 (-> character-grid
                     (get (char->index column-char))
                     (get (char->index row-char))))))
            {}
            (for [c alphabet-chars c2 alphabet-chars] [c c2]))))

(defn encode-decode
  [keyword message invert?]
  (let [invert #(if invert? (clojure.set/map-invert %) %)]
    (string/join (map #(-> alphabet-cipher-chart
                           (get %1)
                           invert
                           (get %2))
                      (cycle (char-array keyword))
                      (char-array message)))))

(defn encode [keyword message] (encode-decode keyword message false))

(defn decode [keyword message] (encode-decode keyword message true))

(defn repeating-substring
  [s]
  (loop [i 0]
    (assert (< i (count s)))
    (let [sub (subs s 0 i)]
      (if (= (string/join (take (count s) (cycle sub))) s)
        sub
        (recur (inc i))))))

(defn decipher
  [cipher message]
  (let [cipher-repeating (decode message cipher)]
    (repeating-substring cipher-repeating)))
