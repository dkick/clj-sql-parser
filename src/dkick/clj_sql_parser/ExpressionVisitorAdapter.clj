(ns dkick.clj-sql-parser.ExpressionVisitorAdapter
  (:require
   [dkick.clj-sql-parser.expression :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.expression.ExpressionVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [[sql-parsed subcontext]
          (visit-before this sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after this sql-parsed context subcontext)))
  context)
