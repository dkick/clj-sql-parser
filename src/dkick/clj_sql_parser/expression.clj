(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [poke]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.expression
    BinaryExpression Function LongValue StringValue)
   (net.sf.jsqlparser.expression.operators.relational
    IsNullExpression ParenthesedExpressionList)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select AllColumns GroupByElement)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-after AllColumns [_ sql-parsed context _]
  (assert (not (-> sql-parsed .getExceptColumns seq)))
  (assert (not (-> sql-parsed .getReplaceExpressions seq)))
  (swap! context conj :*))

(defmethod visit-after BinaryExpression [_ sql-parsed context _]
  (swap! context
         (fn [context']
           (let [operator (-> sql-parsed .getStringExpression keyword)
                 right    (peek context')
                 context' (pop context')
                 left     (peek context')
                 context' (pop context')]
             (conj context' [operator left right])))))

(defmethod visit-after Column [_ sql-parsed context _]
  (swap! context conj (-> sql-parsed .getFullyQualifiedName keyword)))

(defmethod visit-before Function [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after Function [_ sql-parsed context subcontext]
  (swap! context conj
         (with-meta
          (apply conj [(-> sql-parsed .getName keyword)] @subcontext)
          ;; This makes for an easy test in visit-after SelectItem
          {:type :sql-fn})))

(defmethod visit-after GroupByElement [_ _ context _]
  (swap! context (poke #(sqh/group-by %))))

(defmethod visit-after IsNullExpression [_ sql-parsed context _]
  (swap! context (poke #(let [op (if (.isNot sql-parsed) :<> :=)]
                          [op % nil]))))

(defmethod visit-before ParenthesedExpressionList [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ParenthesedExpressionList
  [_ _ context subcontext]
  (swap! context conj [@subcontext]))

;;; We cannot quite believe that there is no base class for all of
;;; these simple value types

(defmethod visit-after LongValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

(defmethod visit-after StringValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))
