(ns dkick.clj-sql-parser.expression
  (:require
   [clojure.string :as str]
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [poke]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.expression
    AnalyticExpression AnalyticType BinaryExpression CaseExpression
    CastExpression Expression Function LongValue NullValue SignedExpression
    StringValue TrimFunction WhenClause)
   (net.sf.jsqlparser.expression.operators.relational
    IsNullExpression ParenthesedExpressionList)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select
    AllColumns GroupByElement OrderByElement)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)
(defmulti visit-order-by multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-after AllColumns [that sql-parsed context _]
  (assert (not (-> sql-parsed .getReplaceExpressions seq)))
  (swap! context conj :*)
  (when-let [except (-> sql-parsed .getExceptColumns seq)]
    (let [except-context (atom [])]
      (doseq [x except]
        (.accept x that except-context))
      (swap! context (poke (fn [x] [x :except @except-context]))))))

(defmethod visit-before AnalyticExpression [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after AnalyticExpression
  [that sql-parsed context subcontext]
  (assert (= (.getType sql-parsed) AnalyticType/OVER))
  (let [analytical-function
        (with-meta
          `[~(-> sql-parsed .getName keyword) ~@(deref subcontext)]
          {:type :sql-fn})

        window-definition
        (let [window-definition-context (atom [])]
          (when-let [x-partition-by (-> sql-parsed
                                        .getWindowDefinition
                                        .getPartitionBy
                                        .getPartitionExpressionList
                                        seq)]
            (let [partition-by-context (atom [])]
              (doseq [x x-partition-by]
                (.accept x that partition-by-context))
              (swap! window-definition-context conj
                     (apply sqh/partition-by @partition-by-context))))
          (when-let [order-by (-> sql-parsed
                                  .getWindowDefinition
                                  .getOrderBy
                                  .getOrderByElements)]
            (let [order-by-context (atom [])]
              (.visitOrderBy that order-by order-by-context)
              (swap! window-definition-context conj
                     (apply sqh/order-by @order-by-context)))))

        ;; TODO From where in the AnalyticalFunction might we get an
        ;; alias?
        alias nil

        args
        (cond-> [analytical-function
                 (apply merge-with into window-definition)]
          alias (conj alias))]

    (swap! context #(apply conj %1 %2) (sqh/over args))))

(defmethod visit-after BinaryExpression [_ sql-parsed context _]
  (swap! context
         (fn [context']
           (let [op       (-> sql-parsed
                              .getStringExpression
                              str/lower-case
                              keyword)
                 right    (peek context')
                 context' (pop context')
                 left     (peek context')
                 context' (pop context')]
             (conj context' [op left right])))))

(defmethod visit-after CastExpression [_ sql-parsed context _]
  (let [col-data-type
        (-> sql-parsed .getColDataType str/lower-case keyword)]
    (swap! context
           (poke (fn [left-expression]
                   [:cast left-expression col-data-type])))))

(defmethod visit-before CaseExpression [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after CaseExpression [_ _ context subcontext]
  ;; expect to find one or more [when then] in the subcontext. the
  ;; else expression is part of the case. we're not sure if that'll be
  ;; in the subcontext or not
  (swap! context conj (with-meta
                        (->> @subcontext
                             (partition 2 2 nil)
                             (mapcat (fn [[x y]]
                                       (if (some? y)
                                         [x y]
                                         [:else x])))
                             (apply conj [:case]))
                        {:type :sql-fn})))

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

(defmethod visit-before GroupByElement [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after GroupByElement [_ _ context subcontext]
  (swap! context conj (apply sqh/group-by @subcontext)))

(defmethod visit-after IsNullExpression [_ sql-parsed context _]
  (swap! context (poke #(let [op (if (.isNot sql-parsed) :<> :=)]
                          [op % nil]))))

(defmethod visit-after LongValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

(defmethod visit-after NullValue [_ _ context _]
  (swap! context conj nil))

(defmethod visit-after ParenthesedExpressionList [_ _ _ _])

(defmethod visit-after SignedExpression [_ _ context _]
  (swap! context (poke #(- %))))

(defmethod visit-after StringValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

(defmethod visit-before TrimFunction [_ sql-parsed _]
  (assert (nil? (.getTrimSpecification sql-parsed)))
  (assert (nil? (.getFromExpression sql-parsed)))
  (assert (not (.isUsingFromKeyword sql-parsed)))
  [(let [args (into-array Expression [(.getExpression sql-parsed)])]
     (Function. "TRIM" args))
   (atom [])])

(defmethod visit-after WhenClause [_ _ _ _])

(defmethod visit-order-by OrderByElement [that sql-parsed context]
  (.accept (.getExpression sql-parsed) that context)
  (swap! context (poke #(sqh/order-by %))))

(defmethod visit-order-by java.util.List [that sql-parsed context]
    (let [subcontext (atom [])]
      (doseq [x sql-parsed]
        (visit-order-by that x subcontext))
      (swap! context conj (apply merge-with into @subcontext))))
