(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution-testing-simple
  (:require [clojure.test :refer :all]
            [clojure.zip :as z]
            [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
            [fox-goose-bag-of-corn.puzzle.approach.zipper-solution :as zt]))



(def dummy-tree-raw-map {:node-val :a
                         :children [{:node-val 4
                                     :children [{:node-val "banana"}
                                                {:node-val "apple"}]}
                                    {:node-val 5
                                     :children [{:node-val "orange"}
                                                {:node-val "mango"}]}]})


(def dummy-tree
  (mapzipper dummy-tree-raw-map))

(def dummy-branch
  (-> dummy-tree z/down z/right z/down z/rightmost))


(deftest zipper-solution-testing-simple
  (testing "dummy-tree-raw-vect"
    (is (=
          ;(count dummy-tree-raw-vect)
          (count dummy-tree-raw-map)
          2)))
  (testing "basic zipper usage"
    (is (=
          (->
            dummy-tree
            z/down
            ;z/right
            ;z/down
            z/node
            :node-val)
          4))
    (is (=
          (-> dummy-tree z/node :node-val)
          :a))
    (is (=
          (-> dummy-branch z/node :node-val)
          "mango"))
    (is (=
          (-> dummy-tree z/down z/down z/rightmost z/node :node-val)
          "apple")))
  (testing "get-lowest-branches"
    (is (=
          (->>
            dummy-tree
            zt/get-lowest-branches
            (map z/node)
            (map :node-val))
          (list "banana" "apple" "orange" "mango"))))
  ;
  (testing "branch->prev-steps"
    (is (=
          (->>
            dummy-branch
            zt/branch->prev-steps)
          (list :a 5 "mango"))))
  (testing "bottom-branch?"
    (is
      (zt/bottom-branch? dummy-branch)))

  (testing "add-branches"
    (is
      (=
        (z/root
          (zt/add-branches dummy-branch [:fruitcup :drink]))
        {:node-val :a, :children [{:node-val 4, :children [{:node-val "banana"} {:node-val "apple"}]} {:node-val 5, :children [{:node-val "orange"} {:node-val "mango", :children [:drink :fruitcup]}]}]}))))
        ;7
;{:node-val :a, :children ({:node-val 4, :children [{:node-val "banana"} {:node-val "apple"}]} {:node-val 5, :children [{:node-val "orange"} {:node-val "mango", :children [:drink :fruitcup]}]})}))))
