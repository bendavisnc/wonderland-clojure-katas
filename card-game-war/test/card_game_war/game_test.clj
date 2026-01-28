(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= (last cards) (play-round (first cards) (last cards)))))
  (testing "queens are higher rank than jacks"
    (is (= [:heart :queen] (play-round [:heart :queen] [:heart :jack]))))
  (testing "kings are higher rank than queens"
    (is (= [:heart :king] (play-round [:heart :king] [:heart :queen]))))
  (testing "aces are higher rank than kings"
    (is (= [:heart :ace] (play-round [:heart :ace] [:heart :king])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (let [[player1-cards player2-cards] (reduce
                                         (fn [[cards-a cards-b] [i card]]
                                           (if (odd? i)
                                             [(conj cards-a card) cards-b]
                                             [cards-a (conj cards-b card)]))
                                         [[] []]
                                         (map-indexed vector cards))
          _ (assert (= [26 26] [(count player1-cards) (count player2-cards)]))
          game-result (play-game player1-cards player2-cards)]
      ;; _ (prn game-result)]
      (is (= 52 (count (:player-one (:cards game-result)))))
      (is (= 0 (count (:player-two (:cards game-result)))))
      (is (= :player-one (:winner game-result))))))
