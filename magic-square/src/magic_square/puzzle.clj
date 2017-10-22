(ns magic-square.puzzle
  (:require [flames.core :as flames], [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

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
            (if
              (= (count lr) (count acc)
                acc)
              ;else
              (let [[x, y] (last acc)]
                (recur (conj acc [(inc x) (dec y)])))))]
          (mapv
            (fn [ps]
              (reduce + (map #(get-in m %) ps)))
            [lr, rl])))


;(defn permutations [values]
;  (let [acc (atom [])]
;    (letfn [
;      (permfn [refvals, size, n]
;        (do
;          (when (= 1 size)
;            (swap! acc conj (apply vector refvals)))
;          (dotimes [i size]
;            (do
;              (permfn refvals (dec size) n)
;              (if (odd? size)
;                (let [t (.get refvals 0)]
;                  (do
;                    (.set refvals 0 (.get refvals (dec size)))
;                    (.set refvals (dec size) t)))
;                ;else
;                (let [t (.get refvals i)]
;                  (do
;                    (.set refvals i (.get refvals (dec size)))
;                    (.set refvals (dec size) t))))))))]
;        (do
;          (permfn (new java.util.ArrayList values) (count values) (count values))
;          @acc))))
;
;boolean nextPermutation(int[] array) {
;                                      // Find longest non-increasing suffix
;                                         int i = array.length - 1;
;                                         while (i > 0 && array[i - 1] >= array[i])
;                                         i--;
;                                      // Now i is the head index of the suffix
;
;                                      // Are we at the last permutation already?
;                                      if (i <= 0)
;                                      return false;
;
;                                      // Let array[i - 1] be the pivot
;                                      // Find rightmost element that exceeds the pivot
;                                      int j = array.length - 1;
;                                      while (array[j] <= array[i - 1])
;                                      j--;
;                                      // Now the value array[j] will become the new pivot
;                                      // Assertion: j >= i
;
;                                      // Swap the pivot with j
;                                      int temp = array[i - 1];
;                                      array[i - 1] = array[j];
;                                      array[j] = temp;
;
;                                      // Reverse the suffix
;                                      j = array.length - 1;
;                                      while (i < j) {
;                                                     temp = array[i];
;                                                     array[i] = array[j];
;                                                          array[j] = temp;
;                                                          i++;
;                                                     j--;
;                                                     }
;
;                                      // Successfully computed the next permutation
;                                      return true;
;                                      }

;(defn next-permutation [a]
;  (let [i (dec (count a))]
;    (while
;      (and
;        (pos? i)
;        (>=
;          (nth a (dec i))
;          (nth a i))))))



(defn is-magical? [s]
  (->
    ((juxt rows-sums, cols-sums, diagonals-sums) s)
    flatten
    set
    count
    (= 1)))


(defn list->sq [l]
  (let [dim (.intValue (Math/sqrt (count l)))]
    (reduce 
      (fn [acc, a]
        (if 
          (= dim (count (last acc))) ; is the latest row already filled up?
            (conj acc [a]) ; start a new row
          ;else ; add to the latest existing row
            (conj (pop acc) (conj (last acc) a))))
      [[]]
      l)))


(defn magic-square* [values] ; brute force attempt
  (let [all-possible-vals (combo/permutations values)]
    (loop [values-set all-possible-vals]
      (let [sq (list->sq (first values-set))]
        (if
          (is-magical? sq)
            sq
          ;else
            (recur (rest values-set)))))))

; (defn magic-square [values]
  ; [[1.0 1.5 2.0]
   ; [2.5 3.0 3.5]
   ; [4.0 4.5 5.0]])

(defn magic-square [values]
  (magic-square* values))


; (defn test-run-basic []
;   (time
;     (println
;       (magic-square values))))

; (defn test-run-advanced []
;   (time
;     (println
;       (magic-square (range 1 26)))))

; (def flames (flames/start! {:port 54321, :host "localhost"}))
; (test-run-basic)

; (defn -main[& args]
  ; (do
    ; (println (permutations [1 2 3 4 5]))))
  ; (test-run-basic))
  ;(test-run-advanced))
