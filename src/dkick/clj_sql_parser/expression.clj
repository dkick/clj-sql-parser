(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select AllColumns)
   (net.sf.jsqlparser.expression ExpressionVisitorAdapter LongValue)))

(defmulti -visit multifn/visit-group)

(defmethod -visit AllColumns [sql-parsed context]
  (assert (-> sql-parsed .getExceptColumns seq nil?))
  (assert (-> sql-parsed .getReplaceExpressions seq nil?))
  (swap! context conj :*))

(defmethod -visit LongValue
  [sql-parsed context]
  (swap! context conj (.getValue sql-parsed)))

(defn expression-visitor []
  (proxy [ExpressionVisitorAdapter] []
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
