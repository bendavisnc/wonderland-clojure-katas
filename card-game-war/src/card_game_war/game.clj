(ns card-game-war.game)

(def suits
  [:spade :club :diamond :heart])

(def ranks
  [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])

(def cards
  (vec (for [rank ranks
             suit suits]
         [rank suit])))


(def player-one ::player-one)
(def player-two ::player-two)
(def discard ::discard)

(def war-cards-count 4)

(defn pure
  [value]
  (fn [s]
    [s value]))

(defn modify
  [f]
  (fn [s]
    [(f s) nil]))

(defn game-over?
  [s]
  (or (empty? (get s player-one))
      (empty? (get s player-two))))

(defn draw-card
  [player]
  (fn [s]
    (let [cards (get s player [])]
      (if (seq cards)
        [(update s player (comp vec rest))
         (first cards)]
        [s nil]))))

(defn draw-cards
  []
  (fn [s]
    (let [[s2 card-a] ((draw-card player-one) s)
          [s3 card-b] ((draw-card player-two) s2)]
      [s3 [card-a card-b]])))

(defn draw-cards-for-war
  []
  (let [draw-for-player
        (fn [player]
          (fn [s]
            (loop [s'  s
                   acc []
                   i   0]
              (if (= i war-cards-count)
                [s' acc]
                (let [[s'' card] ((draw-card player) s')]
                  (recur s''
                         (if card
                           (conj acc card)
                           acc)
                         (inc i)))))))]
    (fn [s]
      (let [[s' cards-a]  ((draw-for-player player-one) s)
            [s'' cards-b] ((draw-for-player player-two) s')]
        [s'' [cards-a cards-b]]))))

(defn draw-discard-pile
  []
  (fn [s]
    (let [cards (get s discard [])
          s2    (assoc s discard [])]
      [s2 cards])))

(defn add-card
  [& {:keys [card-designee card]}]
  (fn [s]
    (assert (vector? (get s card-designee))
            (str [card-designee card s]))
    (let [s2 (update s
                     card-designee
                     (fnil conj [])
                     card)]
      [s2 nil])))

(defn compare-cards
  [player-one-cards player-two-cards]
  (let [card-a (last player-one-cards)
        card-b (last player-two-cards)]
    (cond
      (and card-a card-b)
      (let [rank-a     (first card-a)
            rank-b     (first card-b)
            rank-a-idx (.indexOf ranks rank-a)
            rank-b-idx (.indexOf ranks rank-b)]
        (cond
          (> rank-a-idx rank-b-idx) player-one
          (< rank-a-idx rank-b-idx) player-two
          :else                     nil))

      card-a
      player-one

      card-b
      player-two

      :else
      nil)))

(defn award-cards
  [&
   {:keys [player
           winning-cards
           losing-cards
           tie-result
           winner]}]
  (fn [s]
    (let [[s2 discarded] ((draw-discard-pile) s)
          cards-to-award (concat discarded
                                 winning-cards
                                 losing-cards)

          s3     (reduce
                  (fn [state card]
                    (first
                     ((add-card
                       :card-designee (or player discard)
                       :card card)
                      state)))
                  s2
                  cards-to-award)

          result (cond (and player (seq winning-cards))
                       (winner player (last winning-cards))

                       (nil? player)
                       tie-result

                       :else
                       (throw
                        (ex-info
                         "Cannot determine winner."
                         {:player        player
                          :winning-cards winning-cards
                          :losing-cards  losing-cards})))]
      [s3 result])))

(defn battle-winner
  [& {:keys [player winning-card losing-card]}]
  (award-cards
   :player player
   :winning-cards (if winning-card [winning-card] [])
   :losing-cards (if losing-card [losing-card] [])
   :tie-result ::war
   :winner
   (fn [player card]
     {::battle-winner
      {:player       player
       :winning-card card}})))

(defn play-battle
  [gamestate]
  (fn [s]
    (let [[s2 [card-a card-b]] (gamestate s)]
      (case (compare-cards
             (if card-a [card-a] [])
             (if card-b [card-b] []))

        ::player-one
        ((battle-winner
          :player       player-one
          :winning-card card-a
          :losing-card  card-b)
         s2)

        ::player-two
        ((battle-winner
          :player       player-two
          :winning-card card-b
          :losing-card  card-a)
         s2)

        ;; Tie
        ((battle-winner
          :player       nil
          :winning-card card-b
          :losing-card  card-a)
         s2)))))

(defn war-winner
  [& {:keys [player winning-cards losing-cards]}]
  (award-cards
   :player player
   :winning-cards winning-cards
   :losing-cards losing-cards
   :tie-result ::draw
   :winner
   (fn [player cards]
     {::war-winner
      {:player        player
       :winning-cards cards}})))

(defn play-war
  [gamestate]
  (fn [s]
    (let [[s2 [player-one-cards player-two-cards]]
          (gamestate s)]
      (case (compare-cards player-one-cards player-two-cards)

        ::player-one
        ((war-winner
          :player        player-one
          :winning-cards player-one-cards
          :losing-cards  player-two-cards)
         s2)

        ::player-two
        ((war-winner
          :player        player-two
          :winning-cards player-two-cards
          :losing-cards  player-one-cards)
         s2)

        ;; Tie
        ((war-winner
          :player        nil
          :winning-cards player-two-cards
          :losing-cards  player-one-cards)
         s2)))))

(defn play-battle-with-war-fallback
  [gamestate]
  (fn [s]
    (let [[s2 battle-result]
          ((play-battle gamestate) s)]
      (cond
        (::battle-winner battle-result)
        [s2 battle-result]

        (= battle-result ::war)
        ((play-war (draw-cards-for-war)) s2)

        :else
        (throw
         (ex-info
          "Unexpected battle result."
          {:result battle-result}))))))


(defn play-round
  [card-a card-b]
  (let [[_ result]
        ((play-battle
          (pure [card-a card-b]))
         {player-one []
          player-two []
          discard    []})]
    (if-let [card (some-> result
                          ::battle-winner
                          :winning-card)]
      card
      (throw
       (ex-info
        "Cards are equal."
        {:card-a card-a
         :card-b card-b})))))

(defn play-game
  [gamestate]
  (fn [s]
    (loop [s' s
           i  0]
      (cond
        (>= i 1000)
        (throw
         (ex-info
          "Maximum game hands exceeded."
          {:hands i}))

        (game-over? s')
        [s' nil]

        :else
        (let [[s'' result]
              ((play-battle-with-war-fallback gamestate) s')]
          (if (game-over? s'')
            [s'' result]
            (recur s'' (inc i))))))))
