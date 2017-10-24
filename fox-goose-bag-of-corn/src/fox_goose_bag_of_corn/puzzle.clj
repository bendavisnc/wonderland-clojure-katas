(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]))

(def index {:left-bank 0, :boat 1, :boat-from-left 1, :boat-from-right 1, :right-bank 2})

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


(defn carry-left-to-boat [fgbc-state, carry-item]
  (vec
    (mapv
      (fn [place, place-index]
        (cond
          (= place-index (index :left-bank))                           ; if we're at the left bank
          (clojure.set/difference place #{:you carry-item})            ;   leave the bank with you and the item
          (= place-index (index :boat))                                ; if we're at the boat
          (clojure.set/union place #{:you carry-item})                 ;   get on the boat with the item
          (= place-index (index :right-bank))                          ; else, right bank stays the same
          place))
      fgbc-state
      (range (count fgbc-state)))))

(defn carry-boat-to-right [fgbc-state, carry-item]
  (vec
    (mapv
      (fn [place, place-index]
        (cond
          (= place-index (index :left-bank))                           ; left bank stays the same
          place
          (= place-index (index :boat))                                ; if we're at the boat
          (clojure.set/difference place #{:you carry-item})            ;   get off the boat with the item
          (= place-index (index :right-bank))                          ; if we're on the right bank
          (clojure.set/union place #{:you carry-item})))               ;   get on the right bank with the item
      fgbc-state
      (range (count fgbc-state)))))

(defn carry-boat-to-left [fgbc-state, carry-item]
  (vec
    (mapv
      (fn [place, place-index]
        (cond
          (= place-index (index :left-bank))
          (clojure.set/union place #{:you carry-item})
          (= place-index (index :boat))
          (clojure.set/difference place #{:you carry-item})
          (= place-index (index :right-bank))
          place))
      fgbc-state
      (range (count fgbc-state)))))

(defn carry-right-to-boat [fgbc-state, carry-item]
  (vec
    (mapv
      (fn [place, place-index]
        (cond
          (= place-index (index :left-bank))
          place
          (= place-index (index :boat))
          (clojure.set/union place #{:you carry-item})
          (= place-index (index :right-bank))
          (clojure.set/difference place #{:you carry-item})))
      fgbc-state
      (range (count fgbc-state)))))

(defn carry-item-from [where]
  (cond
    (= where :left-bank)
    carry-left-to-boat
    (= where :boat-from-left)
    carry-boat-to-right
    (= where :boat-from-right)
    carry-boat-to-left
    (= where :right-bank)
    carry-right-to-boat))

(defn every-possible-next-state [prev-states]
  (let [coming-from (find-where-are-you? prev-states),
        most-recent (last prev-states)
        carry-fn (carry-item-from coming-from)]
    (mapv
      (fn [carry-item]
        (carry-fn most-recent, carry-item))
      (clojure.set/difference (nth most-recent (index coming-from)) #{:you :boat}))))


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





(defn test-coming-from-left []
  (let [test-data
        [[[:fox :goose :corn :you] [:boat] []]]]
    (doseq [p
            (sets->vecs
              (every-possible-next-state (vecs->sets test-data)))]
      (println p))))

(defn test-coming-from-right []
  (let [test-data
        [[[:fox :goose] [:boat] [:corn :you]]]]
    (doseq [p
            (sets->vecs
              (every-possible-next-state (vecs->sets test-data)))]
      (println p))))

(defn test-coming-from-boat []
  (let [test-data
        [[[:fox :goose] [:boat] [:corn :you]],
         [[:fox :goose] [:you :boat :corn] []]]]
    (doseq [p
            (sets->vecs
              (every-possible-next-state (vecs->sets test-data)))]
      (println p))))

(defn -main [& args]
  (test-coming-from-left))
