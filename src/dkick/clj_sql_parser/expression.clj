(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.expression BinaryExpression Function LongValue)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select AllColumns)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context] context)

(defmethod visit-after AllColumns [sql-parsed context _]
  (assert (not (-> sql-parsed .getExceptColumns seq)))
  (assert (not (-> sql-parsed .getReplaceExpressions seq)))
  (swap! context conj :*))

(defmethod visit-after BinaryExpression [sql-parsed context _]
  (swap! context
         (fn [context']
           (let [operator (-> sql-parsed .getStringExpression keyword)
                 right    (peek context')
                 context' (pop context')
                 left     (peek context')
                 context' (pop context')]
             (conj context' [operator left right])))))

(defmethod visit-after Column [sql-parsed context _]
  (swap! context conj (-> sql-parsed .getFullyQualifiedName keyword)))

(defmethod visit-before Function [_ _] (atom []))

(defmethod visit-after Function [sql-parsed context subcontext]
  (swap! context conj
         (with-meta
          (apply conj [(-> sql-parsed .getName keyword)] @subcontext)
          ;; This makes for an easy test in visit-after SelectItem
          {:type :sql-fn})))

(defmethod visit-after LongValue [sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))
