(ns fox-goose-bag-of-corn.puzzle-two
  (:require [clojure.set]
            [clojure.pprint]
            [fox-goose-bag-of-corn.puzzle.java-solution :as chosen-solution]))

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn vecs->sets [positions]
  "A vec of vecs of vecs -> a vec of vecs of sets"
  (mapv #(mapv set %) positions))

(defn sets->vecs [positions]
  "A vec of vecs of sets -> a vec of vecs of vecs"
  (mapv #(mapv vec %) positions))


;(defn river-crossing-plan []
;  start-pos)
(defn river-crossing-plan []
  (sets->vecs
    (chosen-solution/river-crossing-plan
      (vecs->sets start-pos))))

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

