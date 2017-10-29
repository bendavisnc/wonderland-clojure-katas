(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set] [clojure.pprint])
  (:import (fox_goose_bag_of_corn.java TreeNode)))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

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
      (fn [g]
        (not (clojure.set/subset? #{:you} g))))
    (filter                  ; ... and does contain an unfaithful pair
      (fn [g-wo-you]
        (or
          (clojure.set/subset?
            #{:fox :goose}
            g-wo-you)
          (clojure.set/subset?
            #{:goose :corn}
            g-wo-you))))
    empty?))                 ; not empty = someone's not safe

(defn boat-capacity-respected? [fgbc-step]
  (->
    fgbc-step
    second
    count
    (<= 3)))

(defn find-where-are-you? [prev-steps]
  (let [most-recent (last prev-steps)]
    (cond
      (clojure.set/subset? #{:you} (first most-recent))
      :left-bank
      (clojure.set/subset? #{:you} (last most-recent))
      :right-bank
      :else ; must be in the boat
      (let [second-most-recent (last (pop prev-steps))]
        (cond
          (clojure.set/subset? #{:you} (first second-most-recent))
          :boat-from-left
          (clojure.set/subset? #{:you} (last second-most-recent))
          :boat-from-right)))))



(defn every-possible-next-from-left-bank [fgbc-step]
  (let [[orig-left, orig-boat, orig-right] fgbc-step]
    (let [
          items-to-carry
          (clojure.set/difference orig-left #{:you})
          every-possible-carry
          (mapv
            (fn [carry-item]
              [
               (clojure.set/difference orig-left #{:you carry-item})
               (clojure.set/union orig-boat #{:you carry-item})
               orig-right])
            items-to-carry)
          no-carry
          [
           (clojure.set/difference orig-left #{:you})
           (clojure.set/union orig-boat #{:you})
           orig-right]]
      (->>
        (conj every-possible-carry no-carry)
        (filter boat-capacity-respected?)
        (filter everyones-safe?)))))


(defn every-possible-next-from-boat-to-right-bank [fgbc-step]
  (let [[orig-left, orig-boat, orig-right] fgbc-step]
    (let [
          items-to-carry
          (clojure.set/difference orig-boat #{:you :boat})
          every-possible-carry
          (mapv
            (fn [carry-item]
              [
               orig-left
               (clojure.set/difference orig-boat #{:you carry-item})
               (clojure.set/union orig-right #{:you carry-item})])
            items-to-carry)
          no-carry
          [
           orig-left
           (clojure.set/difference orig-boat #{:you})
           (clojure.set/union orig-right #{:you})]]
      (->>
        (conj every-possible-carry no-carry)
        (filter boat-capacity-respected?)
        (filter everyones-safe?)))))

(defn every-possible-next-from-right-bank [fgbc-step]
  (let [[orig-left, orig-boat, orig-right] fgbc-step]
    (let [
          items-to-carry
          (clojure.set/difference orig-right #{:you})
          every-possible-carry
          (mapv
            (fn [carry-item]
              [
               orig-left
               (clojure.set/union orig-boat #{:you carry-item})
               (clojure.set/difference orig-right #{:you carry-item})])
            items-to-carry)
          no-carry
          [
           orig-left
           (clojure.set/union orig-boat #{:you})
           (clojure.set/difference orig-right #{:you})]]
      (->>
        (conj every-possible-carry no-carry)
        (filter boat-capacity-respected?)
        (filter everyones-safe?)))))

(defn every-possible-next-from-boat-to-left-bank [fgbc-step]
  (let [[orig-left, orig-boat, orig-right] fgbc-step]
    (let [
          items-to-carry
          (clojure.set/difference orig-boat #{:you :boat})
          every-possible-carry
          (mapv
            (fn [carry-item]
              [
               (clojure.set/union orig-left #{:you carry-item})
               (clojure.set/difference orig-boat #{:you carry-item})
               orig-right])
            items-to-carry)
          no-carry
          [
           (clojure.set/union orig-left #{:you})
           (clojure.set/difference orig-boat #{:you})
           orig-right]]
      (->>
        (conj every-possible-carry no-carry)
        (filter boat-capacity-respected?)
        (filter everyones-safe?)))))

(defn every-possible-next-from [where]
  (
    {
     :left-bank every-possible-next-from-left-bank
     :boat-from-left every-possible-next-from-boat-to-right-bank
     :right-bank every-possible-next-from-right-bank
     :boat-from-right every-possible-next-from-boat-to-left-bank}
    where))


(defn every-possible-next-step [prev-steps]
  (let [coming-from (find-where-are-you? prev-steps),
        most-recent (last prev-steps)
        applicable-fn (every-possible-next-from coming-from)]
    (applicable-fn most-recent)))


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
  (let [lowest-branches (get-lowest-branches tree)]
    (do
      (doseq [branch lowest-branches]
        (add-branches
          branch
          (every-possible-next-step
            (branch->prev-steps branch))))
      tree)))


(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      (fn [^TreeNode node]
        (= (nth (.rootVal node) 2) #{:fox :goose :corn :you})))
    (map
      (fn [^TreeNode golden-node]
        (branch->prev-steps golden-node)))
    first))
;
;
(defn river-crossing-plan* [sp]
  (loop [tree (tree-with-root (first sp))]
    (let [extended-tree (build-up-tree tree)]
      (do
        ;(println (.toPrettyString extended-tree))
        (or
          (found-result extended-tree)
          (recur extended-tree))))))


;(defn river-crossing-plan []
;  start-pos)
;
(defn river-crossing-plan []
  (sets->vecs
    (river-crossing-plan* (vecs->sets start-pos))))


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

