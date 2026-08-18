(ns alphabet-cipher.coder
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]))

(defn alphabet-cipher-chart-next-row
  [row]
  (vec (concat (rest row) [(first row)])))

(defn char->index [c] (- (int c) (int \a)))

(def
  ^{:doc
    "A map of maps where `((thismap b) a) returns the character in the substitution chart according to the row index derived by `b` and the column index derived by `a`."}
  alphabet-cipher-chart
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
  "Applies `alphabet-cipher-chart` to build a new string.
   Characters from index n are taken from `keyword` and `message` as input.
   `invert` is used for decoding so that the value for the secondary map becomes the key."
  [keyword message invert?]
  (let [invert (if invert? clojure.set/map-invert identity)]
    (string/join (map #(-> alphabet-cipher-chart
                           (get %1)
                           invert
                           (get %2))
                      (cycle (char-array keyword))
                      (char-array message)))))

(defn repeating-substring
  "Examines the first character of s, and then the first two, etc until repeating that substring and combining it is equal to `s`.
   Example:
   (repeating-substring \"bananabananabananabanan\")
   \"banana\")"
  [s]
  (loop [i 0]
    (assert (< i (count s)))
    (let [sub (subs s 0 i)]
      (if (= (string/join (take (count s) (cycle sub))) s)
        sub
        (recur (inc i))))))

(defn encode [keyword message] (encode-decode keyword message false))

(defn decode [keyword message] (encode-decode keyword message true))

(defn decipher
  [cipher message]
  (let [cipher-repeating (decode message cipher)]
    (repeating-substring cipher-repeating)))
