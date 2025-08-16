(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.expression ExpressionVisitorAdapter LongValue)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select AllColumns)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context]
  context)

(defmethod visit-after AllColumns [sql-parsed context _]
  (assert (-> sql-parsed .getExceptColumns seq nil?))
  (assert (-> sql-parsed .getReplaceExpressions seq nil?))
  (swap! context conj :*))

(defmethod visit-after Column [sql-parsed context _]
  (swap! context conj (-> sql-parsed .getFullyQualifiedName keyword)))

(defmethod visit-after LongValue [sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

#_(defn expression-visitor []
  (proxy [ExpressionVisitorAdapter] []
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (visit-after sql-parsed context))
      context)))
