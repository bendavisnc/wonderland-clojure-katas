(ns card-game-war.game-tom-and-mary-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(deftest test-play-game-turn-one
  (testing "turn one"
    (let [toms-cards  [[4 :club]]
          marys-cards [[8 :spade]]
          card-hands  {player-one toms-cards player-two marys-cards}
          [card-hands' {:keys [card-game-war.game/battle-winner]}]
          ((play-game (draw-cards)) card-hands)]
      (is (= player-two (:player battle-winner)))
      (is (= (first marys-cards) (:winning-card battle-winner)))
      (is (= 0 (count (player-one card-hands'))))
      (is (= 2 (count (player-two card-hands'))))
      (is (= 0 (count (discard card-hands')))))))


(deftest test-play-game-turn-two
  (testing "turn two"
    (let [toms-cards  [[:ace :spade]]
          marys-cards [[:king :spade]]
          card-hands  {player-one toms-cards player-two marys-cards}
          [card-hands' {:keys [card-game-war.game/battle-winner]}]
          ((play-game (draw-cards)) card-hands)]
      (is (= player-one (:player battle-winner)))
      (is (= (first toms-cards) (:winning-card battle-winner)))
      (is (= 2 (count (player-one card-hands'))))
      (is (= 0 (count (player-two card-hands'))))
      (is (= 0 (count (discard card-hands')))))))


(deftest test-play-game-turn-three
  (testing "turn three"
    (let [toms-cards      [[6 :spade] [2 :spade] [3 :spade] [4 :spade]
                           [8 :diamond]]
          marys-cards     [[6 :diamond] [2 :diamond] [3 :diamond] [4 :diamond]
                           [9 :heart]]
          card-hands      {player-one toms-cards player-two marys-cards}
          [card-hands' _] ((play-game (draw-cards)) card-hands)]
      (is (= 0 (count (player-one card-hands'))))
      (is (= (+ (count toms-cards) (count marys-cards))
             (count (player-two card-hands'))))
      (is (= 0 (count (discard card-hands')))))))





