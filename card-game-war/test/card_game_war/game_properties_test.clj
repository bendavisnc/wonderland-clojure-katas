(ns card-game-war.game-properties-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [card-game-war.game :refer :all]))


(defn shuffle-and-split-gen
  [acc]
  (gen/bind (gen/shuffle acc)
            (fn [shuffled-acc]
              (gen/fmap (fn [n] (split-at n shuffled-acc))
                        (gen/choose 0 (dec (count acc)))))))

(def gen-card-hands
  (gen/fmap (fn [[cards-a cards-b]] {player-one cards-a, player-two cards-b})
            (shuffle-and-split-gen cards)))


(defspec no-cards-missing
         1000
         (prop/for-all [card-hands gen-card-hands]
                       (let [[card-hands _] ((play-game (draw-cards))
                                             card-hands)]
                         ;; After the game, the total number of cards
                         ;; across all hands (including discard)
                         ;; must still be 52.
                         (= 52
                            (->> card-hands
                                 vals
                                 (map count)
                                 (apply +))))))


(defspec one-player-no-cards
         1000
         (prop/for-all
          [card-hands gen-card-hands]
          (let [[card-hands _]   ((play-game (draw-cards)) card-hands)
                players-no-cards (filter (fn [[card-designee cards]]
                                           (if (= discard card-designee)
                                             false
                                             (empty? cards)))
                                         card-hands)]
            ;; After the game, one player should be out of cards.
            (= 1 (count players-no-cards)))))

(alter-var-root
 #'clojure.test.check.clojure-test/assert-check
 (constantly
  (fn [{:keys [result result-data], :as m}]
    (if-let [error (:clojure.test.check.properties/error result-data)]
      (clojure.test/do-report
       {:type     :error,
        :message  (-> m
                      (dissoc :result :result-data)
                      (update :shrunk dissoc :result)
                      (update-in [:shrunk :result-data]
                                 dissoc
                                 :clojure.test.check.properties/error)),
        :expected {:result true},
        :actual   error})
      (clojure.test/is (clojure.test.check.clojure-test/check? m))))))

