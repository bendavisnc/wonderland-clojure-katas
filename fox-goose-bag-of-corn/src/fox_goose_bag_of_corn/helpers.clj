(ns fox-goose-bag-of-corn.helpers)

(defn step-key->index
  [k]
  (let [i (.indexOf [:left-bank :boat :right-bank] k)]
    (if (= -1 i)
      (throw (new IllegalStateException
                  (format "No matching index for key, `%s`" k)))
      i)))

