(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])

(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(def cards (for [suit suits rank ranks] [suit rank]))

(defn play-round
  [player1-card player2-card]
  (let [cards [player1-card player2-card]
        [player1-rank-val player2-rank-val]
        (map (fn [card] (.indexOf ranks (card 1))) cards)]
    (case (compare player1-rank-val player2-rank-val)
      1 player1-card
      -1 player2-card
      0 nil)))

(defn game-winner
  [player1-cards player2-cards]
  (cond (empty? player1-cards) {:winner :player-two
                                :cards {:player-one player1-cards
                                        :player-two player2-cards}}
        (empty? player2-cards) {:winner :player-one
                                :cards {:player-one player1-cards
                                        :player-two player2-cards}}))

(defn round-winner
  [player1-cards player2-cards]
  (let [[p1-topcard & p1-remaining-cards] player1-cards
        _ (assert p1-topcard "No card, player one")
        [p2-topcard & p2-remaining-cards] player2-cards
        _ (assert p2-topcard "No card, player two")
        winner-card (play-round p1-topcard p2-topcard)]
    (cond (= p1-topcard winner-card) [(concat p1-remaining-cards
                                              (list p1-topcard p2-topcard))
                                      p2-remaining-cards]
          (= p2-topcard winner-card) [p1-remaining-cards
                                      (concat p2-remaining-cards
                                              (list p2-topcard p1-topcard))])))

(defn round-winner-tiebreak
  ([player1-cards player2-cards limbo-cards]
   (cond (<= 4 (count player2-cards))
         [(concat player1-cards player2-cards limbo-cards) nil]
         (<= 4 (count player1-cards))
         [nil (concat player2-cards player1-cards limbo-cards)]
         :else
         (let [[p1-topcards p1-remaining-cards]
               ((juxt (partial take 4) (partial drop 4)) player1-cards)
               [p2-topcards p2-remaining-cards]
               ((juxt (partial take 4) (partial drop 4)) player2-cards)
               p1-lasttopcard (last p1-topcards)
               p2-lasttopcard (last p2-topcards)
               winner-card (play-round p1-lasttopcard p2-lasttopcard)]
           (cond
             (= p1-lasttopcard winner-card)
             [(concat p1-remaining-cards p1-topcards p2-topcards limbo-cards)
              p2-remaining-cards]
             (= p2-lasttopcard winner-card)
             [p1-remaining-cards
              (concat p2-remaining-cards p2-topcards p1-topcards limbo-cards)]
             :else (round-winner-tiebreak
                    p1-remaining-cards
                    p2-remaining-cards
                    (concat limbo-cards p1-topcards p2-topcards))))))
  ([player1-cards player2-cards]
   (round-winner-tiebreak player1-cards player2-cards nil)))

(defn play-game
  [player1-cards player2-cards]
  (if-let [winner (game-winner player1-cards player2-cards)]
    winner
    ;; else
    (apply play-game
           (or (round-winner player1-cards player2-cards)
               (round-winner-tiebreak player1-cards player2-cards)))))

(comment
  (play-round [:neat :king] [:spade :king]))

(comment
  (play-round (first cards) (last cards)))

(comment
  ((juxt inc dec) 7))

(comment
  (conj '(1 2) 3))

(comment
  (conj (vector 1 2) 3))

(comment
  (map second
       (first (vals (group-by #(odd? (first %)) (map-indexed vector cards))))))

(comment
  (let [[a & [b & c]] (list :a :b :c)] [a b c]))

(comment
  (assert false "not true"))

(comment
  ((juxt (partial take 3) (partial drop 3)) (list :a :b :c :d :e :f)))