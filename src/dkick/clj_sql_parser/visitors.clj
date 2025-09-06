(ns dkick.clj-sql-parser.visitors)

(defn visit-before-group
  [visitor sql-parsed context]
  (assert (instance? Object visitor))
  (assert (instance? clojure.lang.Atom context))
  (type sql-parsed))

(defn visit-after-group
  [visitor sql-parsed context subcontext]
  (assert (instance? Object visitor))
  (assert (instance? clojure.lang.Atom context))
  (assert (instance? clojure.lang.Atom subcontext))
  (type sql-parsed))
