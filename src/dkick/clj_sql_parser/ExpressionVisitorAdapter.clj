(ns dkick.clj-sql-parser.ExpressionVisitorAdapter
  (:require
   [dkick.clj-sql-parser.expression
    :refer [visit-after visit-before visit-order-by]])
  (:gen-class
   :extends net.sf.jsqlparser.expression.ExpressionVisitorAdapter
   :exposes-methods {visit superVisit}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [[sql-parsed subcontext]
          (visit-before this sql-parsed context)]
      (when sql-parsed
        (.superVisit this sql-parsed subcontext)
        (visit-after this sql-parsed context subcontext))))
  context)

(defn -visitOrderBy [this sql-parsed context]
  (when sql-parsed
    (visit-order-by this sql-parsed context)))
