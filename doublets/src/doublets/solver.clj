(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.set :as clj-set])
  (:import [clojure.lang PersistentQueue]))

(def words
  (-> "words.edn"
      (io/resource)
      (slurp)
      (read-string)))

(defn varies-by-one?
  [word1 word2]
  (and
   (= (count word1) (count word2))
   (= 1 (count (filter (fn [[a b]] (not (= a b))) (map vector word1 word2))))))

(defn doublets
  "Performs a breadth first search with a queue. Each iteration adds n new `word-link-chain`s 
   where `word-link-chain` is a seq of words and n is determined by how many word links can be 
   found for the last word of the chain currently being examined. 
   Returns either a found doublet or an empty vector once the queue is empty."
  [word1 word2]
  (loop [q    (conj PersistentQueue/EMPTY [word1])
         seen #{word1}]
    (let [word-links-chain (peek q)
          word-to-link     (last word-links-chain)
          word-links       (filter (every-pred (partial varies-by-one?
                                                        word-to-link)
                                               (complement seen))
                                   words)]
      (cond (empty? q) []
            (= word-to-link word2) word-links-chain
            :else (recur (into (pop q)
                               (map (fn [word] (conj word-links-chain word))
                                    word-links))
                         (into seen word-links))))))
