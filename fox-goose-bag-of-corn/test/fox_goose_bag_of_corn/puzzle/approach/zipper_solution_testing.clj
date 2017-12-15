(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution-testing
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
            [fox-goose-bag-of-corn.puzzle.approach.zipper-solution :as zt]))




(def dummy-tree
  (mapzipper {:node-val
              [#{:fox :goose :corn :you} #{:boat} #{}]}))


(deftest zipper-solution-testing
  (testing "get-lowest-branches"
    (is (=
          (->>
            dummy-tree
            zt/get-lowest-branches
            (map z/node)
            (map :node-val))
          [[#{:you :fox :goose :corn} #{:boat} #{}]])))

  (testing "branch->prev-steps"
    (is (=
          (->>
            dummy-tree
            zt/branch->prev-steps)
          [[#{:you :fox :goose :corn} #{:boat} #{}]]))))

