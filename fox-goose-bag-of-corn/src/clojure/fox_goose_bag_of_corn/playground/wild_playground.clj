(ns fox-goose-bag-of-corn.playground.wild-playground
  (:require [clojure.zip :as z]
            [clojure.data.zip :as z-util]
            [clojure.walk :as walk]))

(def dummy-tree-raw
  {:node-val :a
   :children [
              {:node-val 4
               :children [{:node-val "banana"}, {:node-val "apple"}]}
              {:node-val 5
               :children [{:node-val "orange"}, {:node-val "mango"}]}]})


(def dummy-tree-raw-vect
  [:a
   [4 "banana" "apple"]
   [5 "orange" "mango"]])

(def dummy-tree-vect
  (z/vector-zip dummy-tree-raw-vect))


(def dummy-tree
  (z/zipper
    ; branch
    (fn [b]
      (and
        b
        (map? b)
        (:node-val b)))
    ; children (unzip)
    (fn [n]
      (:children n))
    ; make node (zip)
    (fn [n c]
      (assoc n :children c))
    dummy-tree-raw))













;;(def dummy-tree
;;  (->
;;    (z/vector-zip [:first])
;;    (z/insert-child [:fourth :second])
;;    (z/insert-child :third)))
;
(def dummy-tree-two
  (z/vector-zip
    [:a
     [4
      ["banana", "apple"]]
     [5
      ["orange", "mango"]]]))
    ;[[[:fifth] :third] [[:fourth] :second] :first]))
;
;
;
;;(defn print-whatnot [t]
;;  (walk/postwalk
;;    (fn [tnode]
;;      (do
;;        (println tnode)
;;        tnode))
;;    t))
;
(defn print-tree [t]
  (if (z/end? t)
    (println "end")
    (do
      (println (or (:node-val (z/node t)) (z/node t)))
      (recur (z/next t)))))
;
(defn bottom? [t]
  (and
    (nil? (z/down t))
    (or
      (nil? (z/right t))
      (not (z/branch? (z/right t))))))

;(defn make-item [t]
;  (loop [acc "", t* t]
;    (cond
;      (nil? (z/up t*)) ; at top?
;      acc
;      (sequential? (z/node t*))
;      (recur
;        acc
;        (-> t* z/up z/leftmost))
;      :default
;      (recur
;        (str acc (:node-val (z/node t*)))
;        (-> t* z/up z/leftmost)))))

;(defn make-item [t]
;  (let [extend-acc #(str %2 (or (:node-val (z/node %1)) (z/node %1)))]
;    (loop [acc "", t* t]
;      (cond
;        (nil? (z/up t*))
;        (extend-acc t* acc)
;        :default
;        (recur
;          (extend-acc t* acc)
;          (z/up t*))))))

(defn make-item [t]
  (clojure.string/join
    (map
      #(or (:node-val (z/node %)) (z/node %))
      (filter
        (fn [t*]
          ;(not (z/branch? t*))
          (not (vector? (z/node t*))))
        (z-util/ancestors t)))))



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
      ;(dummy-tree-parsing dummy-tree)
      (dummy-tree-parsing dummy-tree-two))))




