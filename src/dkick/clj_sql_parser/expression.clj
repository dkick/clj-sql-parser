(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.expression ExpressionVisitorAdapter LongValue)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select AllColumns)))

(defmulti -visit multifn/visit-context-group)

(defmethod -visit AllColumns [sql-parsed context]
  (assert (-> sql-parsed .getExceptColumns seq nil?))
  (assert (-> sql-parsed .getReplaceExpressions seq nil?))
  (swap! context conj :*))

(defmethod -visit Column [sql-parsed context]
  (swap! context conj (-> sql-parsed .getFullyQualifiedName keyword)))

(defmethod -visit LongValue [sql-parsed context]
  (swap! context conj (.getValue sql-parsed)))

(defn expression-visitor []
  (proxy [ExpressionVisitorAdapter] []
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
