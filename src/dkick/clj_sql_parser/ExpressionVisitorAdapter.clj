(ns dkick.clj-sql-parser.ExpressionVisitorAdapter
  (:require
   [dkick.clj-sql-parser.expression :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.expression.ExpressionVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [subcontext (visit-before sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after sql-parsed context subcontext)))
  context)
