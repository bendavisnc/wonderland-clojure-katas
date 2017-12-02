(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]
            [clojure.pprint]
            [clojure.spec.alpha :as spec]
            [fox-goose-bag-of-corn.puzzle.approach.queue-solution :as chosen-solution]
            [clojure.spec.test.alpha :as spec-test]
            [fox-goose-bag-of-corn.puzzle.specs :as common-specs]))

;[fox-goose-bag-of-corn.puzzle.approach.java-solution :as chosen-solution]


(spec/check-asserts true)
(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

;(spec/assert common-specs/step-instance-vec (first start-pos))

(spec/assert common-specs/step-instance-vec (first start-pos))


;[fox-goose-bag-of-corn.puzzle.approach.java-solution :as chosen-solution]
;[fox-goose-bag-of-corn.puzzle.approach.go-solution :as chosen-solution]

(defn vecs->sets [positions]
  "A vec of vecs of vecs -> a vec of vecs of sets"
  (mapv #(mapv set %) positions))

(spec/fdef vecs->sets
           :args (spec/cat :positions (spec/coll-of common-specs/step-instance-vec))
           :ret (spec/coll-of common-specs/step-instance-set))

(defn sets->vecs [positions]
  "A vec of vecs of sets -> a vec of vecs of vecs"
  (mapv #(mapv vec %) positions))

(spec/fdef sets->vecs
           :args (spec/cat :positions (spec/coll-of common-specs/step-instance-set))
           :ret (spec/coll-of common-specs/step-instance-vec))

;(defn river-crossing-plan []
;  start-pos)
(defn river-crossing-plan []
  (sets->vecs
    (chosen-solution/river-crossing-plan
      (vecs->sets start-pos))))


(spec-test/instrument `sets->vecs)
(spec-test/instrument `vecs->sets)

(defn -main [& args]
  (time
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
