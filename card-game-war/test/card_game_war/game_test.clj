(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= (last cards) (play-round (first cards) (last cards)))))
  (testing "queens are higher rank than jacks"
    (is (= [:queen :heart] (play-round [:queen :heart] [:jack :heart]))))
  (testing "kings are higher rank than queens"
    (is (= [:king :heart] (play-round [:king :heart] [:queen :heart]))))
  (testing "aces are higher rank than kings"
    (is (= [:ace :heart] (play-round [:ace :heart] [:king :heart])))))

(deftest test-play-game-hands-are-mirrored
  (testing
    "the player loses when they run out of cards, unless card hands are mirrored"
    (let [[player-one-cards player-two-cards] (reduce
                                               (fn [[cards-a cards-b] [i card]]
                                                 (if (odd? i)
                                                   [(conj cards-a card)
                                                    cards-b]
                                                   [cards-a
                                                    (conj cards-b card)]))
                                               [[] []]
                                               (map-indexed vector cards))
          _ (assert (= [26 26]
                       [(count player-one-cards) (count player-two-cards)]))
          card-hands      {player-one (vec player-one-cards)
                           player-two (vec player-two-cards)
                           discard    []}
          [card-hands' _] ((play-game (draw-cards)) card-hands)]
      (is (= 0 (count (player-one card-hands'))))
      (is (= 0 (count (player-two card-hands'))))
      (is (= 52 (count (discard card-hands')))))))


(deftest test-play-game
  (testing
    "the player loses when they run out of cards"
    (let [[cards-a cards-b] (reduce (fn [[cards-a cards-b] [i card]]
                                      (if (odd? i)
                                        [(conj cards-a
                                               card)
                                         cards-b]
                                        [cards-a
                                         (conj cards-b card)]))
                                    [[] []]
                                    (map-indexed vector cards))
          player-one-cards  (vec (concat [(last cards-b)] cards-a))
          player-two-cards  (vec (drop-last cards-b))
          _ (assert (= [27 25]
                       [(count player-one-cards)
                        (count
                         player-two-cards)]))
          card-hands        {player-one player-one-cards
                             player-two player-two-cards
                             discard    []}
          [card-hands' _]   ((play-game (draw-cards)) card-hands)]
      (is (= 0 (count (player-one card-hands'))))
      (is (= 52 (count (player-two card-hands'))))
      (is (= 0 (count (discard card-hands')))))))

