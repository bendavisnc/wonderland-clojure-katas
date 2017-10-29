(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set] [clojure.pprint])
  (:import (fox_goose_bag_of_corn.java TreeNode)))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(def index {:left-bank 0, :boat 1, :right-bank 2})

(defn vecs->sets [positions]
  "A vec of vecs of vecs -> a vec of vecs of sets"
  (mapv #(mapv set %) positions))

(defn sets->vecs [positions]
  "A vec of vecs of sets -> a vec of vecs of vecs"
  (mapv #(mapv vec %) positions))

(defn everyones-safe? [fgbc-step]
  (->>
    fgbc-step
    (filter                  ; only look at what doesn't have "you"
      #(not (:you %)))
    (filter                  ; ... and does contain an unfaithful pair
      #(or
        (clojure.set/subset?
          #{:fox :goose}
          %)
        (clojure.set/subset?
          #{:goose :corn}
          %)))
    empty?))                ; not empty = someone's not safe

(defn boat-capacity-respected? [fgbc-step]
  (->
    fgbc-step
    second
    count
    (<= 3)))

(defn find-where-are-you? [prev-steps]
  (let [most-recent (last prev-steps)]
    (cond
      (:you (first most-recent))
      :left-bank
      (:you (last most-recent))
      :right-bank
      :else ; must be in the boat
      (let [second-most-recent (last (pop prev-steps))]
        (cond
          (:you (first second-most-recent))
          :boat-from-left
          (:you (last second-most-recent))
          :boat-from-right)))))


(defn every-possible-next-from-* [from, to]
  (fn [fgbc-step]
    (let [
          items-to-carry
          (clojure.set/difference (fgbc-step (index from)) #{:you :boat})
          every-possible-step
          (mapv
            (fn [carry-item]
              (->
                fgbc-step
                (update (index from) #(clojure.set/difference % #{:you carry-item}))
                (update (index to) #(clojure.set/union % (if carry-item #{:you carry-item} #{:you})))))
            (conj items-to-carry nil))]
      (->>
        every-possible-step
        (filter boat-capacity-respected?)
        (filter everyones-safe?)))))

(def every-possible-next-from
  {:left-bank (every-possible-next-from-* :left-bank :boat)
   :boat-from-left (every-possible-next-from-* :boat :right-bank)
   :right-bank (every-possible-next-from-* :right-bank :boat)
   :boat-from-right (every-possible-next-from-* :boat :left-bank)})

(defn every-possible-next-step [prev-steps]
  (let [coming-from (find-where-are-you? prev-steps),
        most-recent (last prev-steps)]
    ((every-possible-next-from coming-from) most-recent)))

(defn tree-with-root [r]
  (new TreeNode r))

(defn get-lowest-branches [^TreeNode tree]
  (.getLowestBranches tree))

(defn add-branches [^TreeNode tree, new-branch-vals]
  (.addBranches tree new-branch-vals))

(defn branch->prev-steps [^TreeNode branch]
  (vec
    (.nodeToValsList branch)))

(defn build-up-tree [tree]
  (doseq [branch (get-lowest-branches tree)]
    (add-branches
      branch
      (every-possible-next-step
        (branch->prev-steps branch)))))

(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      #(= ((.rootVal %) (index :right-bank)) #{:fox :goose :corn :you}))
    (map
      #(branch->prev-steps %))
    first))


(defn river-crossing-plan* [sp]
  (let [tree (tree-with-root (first sp))]
    (loop []
      (do
        (build-up-tree tree)
        (or
          (found-result tree)
          (recur))))))

;(defn river-crossing-plan []
;  start-pos)
(defn river-crossing-plan []
  (sets->vecs
    (river-crossing-plan*
      (vecs->sets start-pos))))

(defn -main [& args]
  (time
    (println
      (clojure.pprint/pprint
        (river-crossing-plan)))))

;[[[:you :fox :goose :corn] [:boat] []]
; [[:fox :corn] [:you :boat :goose] []]
; [[:fox :corn] [:boat] [:you :goose]]
; [[:fox :corn] [:you :boat] [:goose]]
; [[:you :fox :corn] [:boat] [:goose]]
; [[:corn] [:you :fox :boat] [:goose]]
; [[:corn] [:boat] [:you :fox :goose]]
; [[:corn] [:you :boat :goose] [:fox]]
; [[:you :goose :corn] [:boat] [:fox]]
; [[:goose] [:you :boat :corn] [:fox]]
; [[:goose] [:boat] [:you :fox :corn]]
; [[:goose] [:you :boat] [:fox :corn]]
; [[:you :goose] [:boat] [:fox :corn]]
; [[] [:you :boat :goose] [:fox :corn]]
; [[] [:boat] [:you :fox :goose :corn]]]
;nil
;"Elapsed time: 130.056998 msecs"

