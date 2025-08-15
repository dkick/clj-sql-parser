(ns dkick.clj-sql-parser.multifn)

(defn visit-context-group
  [sql-parsed context]
  (assert (instance? clojure.lang.Atom context))
  (type sql-parsed))

(defn visit-subcontext-group
  [sql-parsed context subcontext]
  (assert (instance? clojure.lang.Atom context))
  (assert (instance? clojure.lang.Atom subcontext))
  (type sql-parsed))
