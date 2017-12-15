(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution-testing
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [fox-goose-bag-of-corn.puzzle.approach.zipper-solution :as zt]))


(def dummy-tree-raw-vect
  [:a
   [4 "banana" "apple"]
   [5 "orange" "mango"]])

(def dummy-tree
  (z/vector-zip dummy-tree-raw-vect))

(def dummy-branch
  (-> dummy-tree z/down z/right z/right z/down z/rightmost))

(deftest zipper-solution-testing
  (testing "dummy-tree-raw-vect"
    (is (=
          (count dummy-tree-raw-vect)
          3)))
  (testing "basic zipper usage"
    (is (=
          (->
            dummy-tree
            z/down
            z/right
            z/down
            z/node)
          4))
    (is (=
          (-> dummy-tree z/down z/node)
          :a))
    (is (=
          (-> dummy-branch z/node)
          "mango"))
    (is (=
          (-> dummy-tree z/down z/right z/down z/rightmost z/node)
          "apple")))
  (testing "get-lowest-branches"
    (is (=
          (->>
            dummy-tree
            zt/get-lowest-branches
            (map z/node))
          (list "banana" "apple" "orange" "mango"))))

  (testing "branch->prev-steps"
    (is (=
          (->>
            dummy-branch
            zt/branch->prev-steps)
          (list :a 5 "mango")))))

