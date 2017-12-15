(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]
            [clojure.pprint]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.approach.java-solution]
            [fox-goose-bag-of-corn.puzzle.approach.go-solution]
            [fox-goose-bag-of-corn.puzzle.approach.teachers-answer]
            [fox-goose-bag-of-corn.puzzle.approach.queue-solution]
            [fox-goose-bag-of-corn.puzzle.approach.zipper-solution]
            [orchestra.spec.test :as spec-test]
            [fox-goose-bag-of-corn.puzzle.specs.puzzle :as puzzle-specs]))


;(def ^:dynamic river-crossing-plan* fox-goose-bag-of-corn.puzzle.approach.queue-solution/river-crossing-plan)
;(def ^:dynamic river-crossing-plan* fox-goose-bag-of-corn.puzzle.approach.java-solution/river-crossing-plan)
(def ^:dynamic river-crossing-plan* fox-goose-bag-of-corn.puzzle.approach.zipper-solution/river-crossing-plan)
;(def ^:dynamic river-crossing-plan* fox-goose-bag-of-corn.puzzle.approach.teachers-answer/river-crossing-plan)

(defmacro with-plan [pfn form]
  `(binding [river-crossing-plan* ~pfn]
    ~form))


(spec/check-asserts true)
(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

;(spec/assert common-specs/step-instance-vec (first start-pos))

(spec/assert puzzle-specs/step-instance-vec (first start-pos))


(defn vecs->sets [positions]
  "A vec of vecs of vecs -> a vec of vecs of sets"
  (mapv #(mapv set %) positions))

(spec/fdef vecs->sets
           :args (spec/cat :positions (spec/coll-of puzzle-specs/step-instance-vec))
           ;:args (spec/cat :positions (spec/coll-of double?))
           :ret (spec/coll-of puzzle-specs/step-instance-set))

(defn sets->vecs [positions]
  "A vec of vecs of sets -> a vec of vecs of vecs"
  (mapv #(mapv vec %) positions))

(spec/fdef sets->vecs
           :args (spec/cat :positions (spec/coll-of puzzle-specs/step-instance-set))
           :ret (spec/coll-of puzzle-specs/step-instance-vec))

(defn river-crossing-plan []
  (sets->vecs
    (river-crossing-plan*
      (vecs->sets start-pos))))


(spec-test/instrument)

(defn -main [& args]
  (time
    ;(binding [chosen-solution 'fox-goose-bag-of-corn.fox-goose-bag-of-corn.puzzle.specs.puzzlee.approach.go-solution]
      (println
        (clojure.pprint/pprint
          (river-crossing-plan)))))

;[[[:you :fox :goose :corn] [:boat] []]
; [[:fox :corn] [:you :boat :goose] []]
; [[:fox :corn] [:boat] [:you :goose]]
; [[:fox :corn] [:you :boat] [:goose]]
; [[:you :fox :corn] [:boat] [:goose]]
; [[:corn] [:you :fox :boat] [:goose]]
; [[:corn] [:boat] [:you :fox :goose]]
; [[:corn] [:you :boat :goose] [:fox]]
; [[:you :goose :corn] [:boat] [:fox]]
; [[:goose] [:you :boat :corn] [:fox]]
; [[:goose] [:boat] [:you :fox :corn]]
; [[:goose] [:you :boat] [:fox :corn]]
; [[:you :goose] [:boat] [:fox :corn]]
; [[] [:you :boat :goose] [:fox :corn]]
; [[] [:boat] [:you :fox :goose :corn]]]
;nil
;"Elapsed time: 130.056998 msecs"
