(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.expression ExpressionVisitorAdapter LongValue)))

(defmulti -visit multifn/visit-group)

(defmethod -visit LongValue
  [sql-parsed context]
  (swap! context conj (.getValue sql-parsed)))

(def expression-visitor
  (proxy [ExpressionVisitorAdapter] []
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
