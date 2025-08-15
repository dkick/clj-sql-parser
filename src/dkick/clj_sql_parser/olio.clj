(ns dkick.clj-sql-parser.olio)

(defn iff-first [xs]
  {:pre [(seq xs)]}
  (let [[car & cdr] xs]
    (assert (nil? cdr))
    car))

(defn poke
  ([f] #(poke % f))
  ([xs f]
   (cond
     (seq xs) (conj (pop xs) (f (peek xs)))
     :else    xs)))
