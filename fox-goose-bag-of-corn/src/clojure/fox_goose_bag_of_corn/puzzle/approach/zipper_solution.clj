(ns fox-goose-bag-of-corn.puzzle.approach.zipper-solution
  (:require
    [fox-goose-bag-of-corn.puzzle.utilities :refer :all]
    [fox-goose-bag-of-corn.puzzle.specs.puzzle :as puzzle-specs]
    [fox-goose-bag-of-corn.puzzle.specs.tree :as tree-specs]
    [clojure.spec.alpha :as spec]
    [clojure.zip :as z]
    ;[clojure.spec.test.alpha :as spec-test]
    [fox-goose-bag-of-corn.puzzle.step-generation :as steps]
    [fox-goose-bag-of-corn.puzzle.logic :as logically]
    [clojure.pprint :as pprint]
    [clojure.walk :as walk]
    [clojure.data.zip :as z-util]
    [orchestra.spec.test :as spec-test]))



(defn bottom-branch? [n]
  (and
    (not (z/down n))))
    ;(not (z/branch? n))))

(spec/fdef bottom-branch?
           :args (spec/cat :n tree-specs/tree)
           :ret boolean?)

;
;(defn bottom? [t]
;  (and
;    (nil? (z/down t))
;    (or
;      (nil? (z/right t))
;      (not (z/branch? (z/right t))))))
;
;(defn make-item [t]
;  (clojure.string/join
;    (map
;      #(or (:node-val (z/node %)) (z/node %))
;      (filter
;        (fn [t*]
;          (not (vector? (z/node t*))))
;        (z-util/ancestors t)))))


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
  (loop [acc () t* tree]
    (cond
      (nil? (z/up t*))
      (into [] (conj acc (:node-val (z/node t*))))
      :default
      (recur
        (conj acc (:node-val (z/node t*)))
        (z/up t*)))))
  ;(mapv
  ;  :node-val
  ;  (conj
  ;    (or (z/path tree) [])
  ;    (z/node tree))))

(spec/fdef branch->prev-steps
           :args (spec/cat :tree tree-specs/tree))
           ;:ret puzzle-specs/step-instance-collection-set)


(defn found-result [tree]
  (->>
    (get-lowest-branches tree)
    (filter
      (fn [n]
        (logically/result-found? (:node-val (z/node n)))))
      ;#(logically/result-found? (z/node %)))
    (map
      #(branch->prev-steps %))
    first))

;(spec/def ::tree tree?)

(spec/fdef found-result
           :args (spec/cat :tree tree-specs/tree)
           :ret (spec/or
                  :nil nil?
                  :result puzzle-specs/step-instance-collection-set))
                  ;:tree tree-specs/tree))

(defn add-branches [tree next-steps]
  (if (empty? next-steps)
    tree
    (recur
      ;(z/insert-right tree (first next-steps))
      (z/insert-child tree (first next-steps))
      (rest next-steps))))

(spec/fdef add-branches
           ;:args (spec/cat :tree tree-specs/tree :next-steps puzzle-specs/step-instance-collection-set)
           :args (spec/cat :tree tree-specs/tree :next-steps coll?)
           :ret tree-specs/tree)


;(defn next-extended [tree steps]
;  (->
;    (update-in
;      tree
;      [0 :children]
;      (fn [e]
;        (into
;          (or e [])
;          steps)))
;        ;(conj (or e []) steps)))
;    z/root
;    mapzipper))

;(defn next-extended [tree steps]
;  (->
;    tree
;    (z/edit
;      (fn [n]
;        (update-in
;          n
;          [0 :children]
;          (fn [e]
;            (into
;              (or e [])
;              steps)))))))


(defn next-extended [tree steps]
  (cond
    (empty? steps)
    tree
    :default
    (recur
      (z/insert-child tree (first steps))
      (rest steps))))

  ;(->
  ;  tree
  ;  (z/edit
  ;    (fn [n]
  ;      (update-in
  ;        n
  ;        [0 :children]
  ;        (fn [e]
  ;          (into
  ;            (or e [])
  ;            steps)



  ;(let [ext
  ;      (-> tree
  ;        (z/replace
  ;          (add-branches tree steps)))]
  ;  (cond
  ;    (z/end? ext)
  ;    ext
  ;    (-> ext z/up z/right)
  ;    (-> ext z/up z/right)
  ;    (-> ext z/up)
  ;    (-> ext z/up)
  ;    :default
  ;    ext)))
    ;z/down))

(spec/fdef next-extended
           ;:args (spec/cat :tree tree-specs/tree :steps puzzle-specs/step-instance-collection-set)
           :args (spec/cat :tree tree-specs/tree :steps coll?)
           :ret tree-specs/tree)

(defn fn-replace [n]
  {:node-val
    #(next-extended n (steps/every-possible-next-step (branch->prev-steps n)))})

(defn expand-tree-phase1 [tree]
  (loop [n tree]
    (cond
      (z/end? n)
      ;(root n)
      (mapzipper (z/root n))
      (bottom-branch? n)
      (recur
        (-> n
          (z/replace (fn-replace n))
          z/next))
      :default
      (recur (z/next n)))))

(spec/fdef expand-tree-phase1
           :args (spec/cat :tree tree-specs/tree)
           :ret tree-specs/tree)


(defn expand-tree-phase2 [tree]
  (loop [n tree]
    (cond
      (z/end? n)
      ;(root n)
      (mapzipper (z/root n))
      (fn? (:node-val (z/node n)))
      (recur
        (z/replace
          n
          (z/node ((:node-val (z/node n))))))
      :default
      (recur (z/next n)))))

(spec/fdef expand-tree-phase2
           :args (spec/cat :tree tree-specs/tree)
           :ret tree-specs/tree)


(defn expand-tree [tree]
  (expand-tree-phase2
    (expand-tree-phase1 tree)))

(spec/fdef expand-tree
           :args (spec/cat :tree tree-specs/tree)
           :ret tree-specs/tree)

(defn river-crossing-plan [sp]
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
        [#{:fox :goose :corn :you} #{:boat} #{}]))))
