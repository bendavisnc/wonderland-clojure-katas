(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn vecs->sets [positions]
  (mapv #(mapv set %) positions))

(defn sets->vecs [positions]
  (mapv #(mapv vec %) positions))

(defn everyones-safe? [fgbc-state]
  (->>
    (mapv set fgbc-state)    ; turn everything into sets
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

(defn find-where-are-you? [prev-states]
  (let [most-recent (last prev-states)]
    (cond
      (clojure.set/subset? #{:you} (first most-recent))
      :left-bank
      (clojure.set/subset? #{:you} (last most-recent))
      :right-bank
      :else ; must be in the boat
      (let [second-most-recent (last (pop prev-states))]
        (cond
          (clojure.set/subset? #{:you} (first second-most-recent))
          :boat-from-left
          (clojure.set/subset? #{:you} (last second-most-recent))
          :boat-from-right)))))


(defn carry-right [fgbc-state carry-item]
  (vec
    (mapv
      (fn [place, place-index]
        (cond
          (= place-index 0)
          (clojure.set/difference (set place) #{:you carry-item})
          (= place-index 1)
          (clojure.set/union (set place) #{:you carry-item})
          (= place-index 2)
          place))
      fgbc-state
      (range (count fgbc-state)))))

(defmulti every-possible-next-state find-where-are-you?)

(defmethod every-possible-next-state :left-bank
  [prev-states]
  (let [most-recent (last prev-states)]
    (mapv
      (fn [carry-item]
        (carry-right most-recent, carry-item))
      (clojure.set/difference (first most-recent) #{:you}))))




    ;(mapv
    ;  (fn [carry-item]
    ;    [
    ;     (clojure.set/difference most-recent))))




(defn river-crossing-plan []
  start-pos)


; [[:fox :goose :corn :you] [:boat] []] ; take the goose first
; [[:fox :corn] [:boat :you :goose] []]
; [[:fox :corn] [:boat] [:you :goose]]
; [[:fox :corn] [:boat :you] [:goose]]
; [[:fox :corn :you] [:boat] [:goose]] ; once back
; [[:corn] [:boat :you :fox] [:goose]] ; take the fox
; [[:corn] [:boat] [:goose :you :fox]] ; then bring the goose back
; [[:corn] [:boat :you :goose] [:fox]]
; [[:corn :you :goose] [:boat] [:fox]] ; then take the corn
; [[:goose] [:boat :corn :you] [:fox]]
; [[:goose] [:boat] [:fox :corn :you]]
; [[:goose] [:boat :you] [:fox :corn]]
; [[:goose :you] [:boat] [:fox :corn]]
; [[] [:boat :goose :you] [:fox :corn]]
; [[] [:boat] [:fox :corn :goose :you]]



(defn -main [& args]
  (doseq [p
          (sets->vecs
            (every-possible-next-state (vecs->sets start-pos)))]
    (println p)))



