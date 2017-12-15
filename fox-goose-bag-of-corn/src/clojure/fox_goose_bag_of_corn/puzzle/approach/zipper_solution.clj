(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution
  (:require
    [fox-goose-bag-of-corn.puzzle.specs :as common-specs]
    [clojure.spec.alpha :as spec]
    [clojure.zip :as z]
    ;[clojure.spec.test.alpha :as spec-test]
    [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
    [fox-goose-bag-of-corn.puzzle.logic :as logically]
    [clojure.pprint :as pprint]
    [clojure.walk :as walk]
    [clojure.data.zip :as z-util]
    [orchestra.spec.test :as spec-test]))


;(defn bottom-branch? [n]
;  (if (nil? n)
;    nil
;    (nil? (z/down n))))
;

(defn bottom-branch? [n]
  (and
    (not (z/branch? n))
    (not
      (= n
         (-> n z/leftmost)))))


(defn- tree? [t]
  (and
    (vector t)
    (= 2 (count t))
    (get-in t [0 :node-val])))
    ;(map? t)
    ;(:node-val t)))

(defn mapzipper [m]
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
    m))













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




(defn get-lowest-branches [tree]
  (loop [acc []
         n tree]
    (cond
      (z/end? n)
      acc
      (bottom-branch? n)
      (recur (conj acc n) (z/next n))
      :default
      (recur acc (z/next n)))))


(defn branch->prev-steps [tree]
  (conj
    (mapv
      z/node
      (z/path tree))
    (z/node tree)))


(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      (fn [n]
        (logically/result-found? (z/node n))))
      ;#(logically/result-found? (z/node %)))
    (map
      #(branch->prev-steps %))
    first))

;(spec/def ::tree tree?)

(spec/fdef found-result
           :args (spec/cat :tree tree?))

(defn add-branches [tree next-steps]
  (if (empty? next-steps)
    tree
    (recur
      (z/insert-right tree (first next-steps))
      (rest next-steps))))

;(defn tree-walker-fn [n]
;  (cond
;    (bottom-branch? n)
;    (add-branches
;      n
;      (steps/every-possible-next-step (branch->prev-steps n)))
;    :default
;    n))
;;(loop [current-node tree]
;
;(defn expanded-tree [tree]
;  (walk/postwalk
;    tree-walker-fn
;    tree))

(defn next-extended [tree steps]
  (->
    tree
    (z/edit
      tree
      (fn [t]
        (add-branches t steps)))
    z/up
    z/right
    z/down))

(spec/fdef next-extended
           :args (spec/cat :tree tree?)
           :ret tree?)

(defn root
  [loc]
  (if (= :end (loc 1))
    loc
    (let [p (z/up loc)]
      (if p
        (recur p)
        loc))))


(defn expand-tree [tree]
  (loop [n tree]
    (cond
      (z/end? n)
      (root n)
      ;(z-util/ancestors)
      (bottom-branch? n)
      (recur
        (next-extended n (steps/every-possible-next-step (branch->prev-steps n))))
      :default
      (recur (z/next n)))))

(spec/fdef expand-tree
           :args (spec/cat :tree tree?)
           :ret tree?)

(defn river-crossing-plan [sp]
  ;(loop [simple-t (z/vector-zip [sp])])
  (loop [simple-t (mapzipper {:node-val sp})]
    (or
      (found-result simple-t)
      (recur
        (expand-tree simple-t)))))

(spec-test/instrument)

(defn -main [& args]
  (time
    (pprint/pprint
      (river-crossing-plan
        [[#{:fox :goose :corn :you} #{:boat} #{}]]))))
