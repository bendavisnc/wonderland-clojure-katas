(ns card-game-war.game)

(def suits [:spade :club :diamond :heart])

(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(def cards (vec (for [rank ranks suit suits] [rank suit])))


(def player-one ::player-one)
(def player-two ::player-two)
(def discard ::discard)

(def war-cards-count 4)

(defn pure [value] (fn [s] [s value]))

(defn modify [f] (fn [s] [(f s) nil]))

(defn flatmap
  [state-fn state-fn-fn]
  (fn [s] (let [[s' a] (state-fn s)] ((state-fn-fn a) s'))))

(defn fmap [state-fn f] (flatmap state-fn (fn [a] (pure (f a)))))

(defn state-fns-sequence
  [& state-fns]
  (reduce (fn [state-fn-a state-fn-b]
            (flatmap state-fn-a
                     (fn [a] (flatmap state-fn-b (fn [b] (pure (conj a b)))))))
          (pure [])
          state-fns))

(defn game-over?
  [s]
  (or (empty? (get s player-one)) (empty? (get s player-two))))

(defn draw-card
  [player]
  (fn [s]
    (let [cards (get s player [])]
      (if (seq cards)
        [(update s player (comp vec rest)) (first cards)]
        [s nil]))))

(defn draw-cards
  []
  (state-fns-sequence (draw-card player-one) (draw-card player-two)))

(defn draw-cards-for-war
  []
  (let [draw-for-player (fn [player]
                          (fmap (apply state-fns-sequence
                                       (map draw-card
                                            (repeat war-cards-count player)))
                                #(filterv seq %)))]
    (->> [player-one player-two]
         (map draw-for-player)
         (apply state-fns-sequence))))

(defn draw-discard-pile
  []
  (fn [s] (let [cards (get s discard []) s2 (assoc s discard [])] [s2 cards])))

(defn add-card
  [& {:keys [card-designee card]}]
  (fn [s]
    (assert (vector? (get s card-designee)) (str [card-designee card s]))
    (let [s2 (update s card-designee (fnil conj []) card)] [s2 nil])))

(defn compare-cards
  [player-one-cards player-two-cards]
  (let [card-a (last player-one-cards)
        card-b (last player-two-cards)]
    (cond (and card-a card-b) (let [rank-a     (first card-a)
                                    rank-b     (first card-b)
                                    rank-a-idx (.indexOf ranks rank-a)
                                    rank-b-idx (.indexOf ranks rank-b)]
                                (cond (> rank-a-idx rank-b-idx) player-one
                                      (< rank-a-idx rank-b-idx) player-two
                                      :else                     nil))
          card-a player-one
          card-b player-two
          :else  nil)))

(defn award-cards
  [& {:keys [player winning-cards losing-cards tie-result winner]}]
  (flatmap (draw-discard-pile)
           (fn [discarded]
             (let [cards-to-award (concat discarded winning-cards losing-cards)]
               (fmap (apply state-fns-sequence
                            (map (fn [card]
                                   (add-card :card-designee (or player discard)
                                             :card card))
                                 cards-to-award))
                     (fn [_]
                       (cond (and player (seq winning-cards))
                             (winner player (last winning-cards))
                             (nil? player) tie-result
                             :else (throw (ex-info "Cannot determine winner."
                                                   {:player player
                                                    :winning-cards winning-cards
                                                    :losing-cards
                                                    losing-cards})))))))))

(defn battle-winner
  [& {:keys [player winning-card losing-card]}]
  (award-cards :player        player
               :winning-cards (if winning-card [winning-card] [])
               :losing-cards  (if losing-card [losing-card] [])
               :tie-result    ::war
               :winner        (fn [player card]
                                {::battle-winner {:player       player
                                                  :winning-card card}})))

(defn play-battle
  [gamestate]
  (flatmap
   gamestate
   (fn [[card-a card-b]]
     (case (compare-cards (if card-a [card-a] []) (if card-b [card-b] []))
       ::player-one (battle-winner :player       player-one
                                   :winning-card card-a
                                   :losing-card  card-b)
       ::player-two (battle-winner :player       player-two
                                   :winning-card card-b
                                   :losing-card  card-a)
       ;; Tie
       (battle-winner :player nil :winning-card card-b :losing-card card-a)))))

(defn war-winner
  [& {:keys [player winning-cards losing-cards]}]
  (award-cards :player        player
               :winning-cards winning-cards
               :losing-cards  losing-cards
               :tie-result    ::draw
               :winner        (fn [player cards]
                                {::war-winner {:player        player
                                               :winning-cards cards}})))

(defn play-war
  [gamestate]
  (flatmap gamestate
           (fn [[player-one-cards player-two-cards]]
             (case (compare-cards player-one-cards player-two-cards)
               ::player-one (war-winner :player        player-one
                                        :winning-cards player-one-cards
                                        :losing-cards  player-two-cards)
               ::player-two (war-winner :player        player-two
                                        :winning-cards player-two-cards
                                        :losing-cards  player-one-cards)
               ;; Tie
               (war-winner :player        nil
                           :winning-cards player-two-cards
                           :losing-cards  player-one-cards)))))

(defn play-battle-with-war-fallback
  [gamestate]
  (flatmap
   (play-battle gamestate)
   (fn [battle-result]
     (cond (::battle-winner battle-result) (pure battle-result)
           (= battle-result ::war)         (play-war (draw-cards-for-war))
           :else                           (throw (ex-info
                                                   "Unexpected battle result."
                                                   {:result battle-result}))))))


(defn play-round
  [card-a card-b]
  (let [[_ result] ((play-battle (pure [card-a card-b]))
                    {player-one [] player-two [] discard []})]
    (if-let [card (some-> result
                          ::battle-winner
                          :winning-card)]
      card
      (throw (ex-info "Cards are equal." {:card-a card-a :card-b card-b})))))

(defn play-game
  [gamestate]
  (fn [s]
    (loop [s' s
           i  0]
      (cond (>= i 1000)     (throw (ex-info "Maximum game hands exceeded."
                                            {:hands i}))
            (game-over? s') [s' nil]
            :else           (let [[s'' result] ((play-battle-with-war-fallback
                                                 gamestate)
                                                s')]
                              (if (game-over? s'')
                                [s'' result]
                                (recur s'' (inc i))))))))