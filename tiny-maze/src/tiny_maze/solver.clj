(ns tiny-maze.solver)

;;
;;
;; Assumptions: 
;;   opening pos = [0, 0]

;;
;; Maze symbols
(def opening 0)
(def wall 1)
(def start :S)
(def end :E)
(def trek :x)

;;
;; Adjacent position functions
(defn left-pos [pos]
  (update pos 1 dec))
(defn right-pos [pos]
  (update pos 1 inc))
(defn top-pos [pos]
  (update pos 0 dec))
(defn bottom-pos [pos]
  (update pos 0 inc))

(defn opening? 
  "Returns pos if pos is a valid open space, else nil."
  [maze pos]
  (and
    (#{start end opening} (get-in maze pos))
    pos))

(defn go-whichway [pos-fn, maze, pos]
  (when-let [open-pos (opening? maze (pos-fn pos))]
    [
      (assoc-in
        maze
        open-pos
        trek),
      open-pos
    ]))
(def go-left (partial go-whichway left-pos))
(def go-right (partial go-whichway right-pos))
(def go-top (partial go-whichway top-pos))
(def go-bottom (partial go-whichway bottom-pos))

(defn solved-maze? [maze]
  (->
    maze
    flatten
    set
    (contains? end)
    not))

(defn go-everywhichway 
  "Return a list of each possible next move.
   The returned vector is of the form [[maze-after-pos-taken, pos-taken] ...]."
  [maze, pos] 
  (remove nil?
    ((juxt go-left, go-right, go-top, go-bottom) maze pos)))

(defn solve-maze* [acc]
  (if-let [solution (first (filter solved-maze? (map first acc)))]
    solution
    ;else
    (if (empty? acc)
      nil
      ;else
      (solve-maze* 
        (mapcat
          (fn [[maze, pos]]
            (go-everywhichway maze pos))
          acc)))))

(defn solve-maze [maze]
  (solve-maze*
    (go-everywhichway maze [-1, 0])))

