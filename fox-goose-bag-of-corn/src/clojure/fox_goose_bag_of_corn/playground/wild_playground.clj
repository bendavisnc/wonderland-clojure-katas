(ns fox-goose-bag-of-corn.playground.wild-playground
  (:require [clojure.zip :as z]
            [clojure.walk :as walk]))


;(def dummy-tree
;  (->
;    (z/vector-zip [:first])
;    (z/insert-child [:fourth :second])
;    (z/insert-child :third)))

(def dummy-tree
  (z/vector-zip
    [:a
     [4
      ["banana", "apple"]]
     [5
      ["orange", "mango"]]]))
    ;[[[:fifth] :third] [[:fourth] :second] :first]))



;(defn print-whatnot [t]
;  (walk/postwalk
;    (fn [tnode]
;      (do
;        (println tnode)
;        tnode))
;    t))

(defn print-tree [t]
  (if (z/end? t)
    (println "end")
    (do
      (println (z/node t))
      (recur (z/next t)))))

(defn bottom? [t]
  (and
    (nil? (z/down t))
    (or
      (nil? (z/right t))
      (not (z/branch? (z/right t))))))

(defn make-item [t]
  (loop [acc "", t* t]
    (cond
      (nil? (z/up t*))
      acc
      (sequential? (z/node t*))
      (recur
        acc
        (-> t* z/up z/leftmost))
      :default
      (recur
        (str acc (z/node t*))
        (-> t* z/up z/leftmost)))))

(defn dummy-tree-parsing [tree]
  (loop [acc [], t tree]

    (cond
      (z/end? t)
      acc

      (bottom? t)
      (recur
        (conj acc (make-item t))
        (z/next t))

      :default
      (recur
        acc
        (z/next t)))))

(defn -main [& args]
  (time
    (println
      (dummy-tree-parsing dummy-tree))))




