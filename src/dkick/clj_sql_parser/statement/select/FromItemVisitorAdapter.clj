(ns dkick.clj-sql-parser.statement.select.FromItemVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select.from-item
    :refer [visit-after visit-before #_visit-joins]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.select.FromItemVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [[sql-parsed subcontext]
          (visit-before this sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after this sql-parsed context subcontext)))
  context)

#_(defn -visitJoins [this joins context]
  ;; No need to invoke the superclass visitJoins. There doesn't seem
  ;; to be a join visitor and we're completely replacing the
  ;; SelectVisitor definition
  (when joins
    (visit-joins this joins context))
  context)
