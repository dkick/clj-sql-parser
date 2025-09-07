(ns dkick.clj-sql-parser.expression
  (:require
   [clojure.string :as str]
   [dkick.clj-sql-parser.visitors :as visitors]
   [dkick.clj-sql-parser.olio :refer [poke]]
   [dkick.clj-sql-parser.schema :refer [get-fully-qualified-name]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.expression
    AllValue AnalyticExpression AnalyticType ArrayExpression BinaryExpression
    BooleanValue CaseExpression CastExpression DoubleValue Expression
    Function LongValue NullValue SignedExpression StringValue
    TimeKeyExpression TrimFunction WhenClause)
   (net.sf.jsqlparser.expression.operators.relational
    InExpression IsBooleanExpression IsNullExpression
    ParenthesedExpressionList)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select
    AllColumns AllTableColumns GroupByElement OrderByElement ParenthesedSelect
    Select)))

(defmulti visit-after visitors/visit-after-group)
(defmulti visit-before visitors/visit-before-group)
(defmulti visit-order-by visitors/visit-before-group)

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

(defmethod visit-after AllTableColumns [that sql-parsed context _]
  (assert (not (-> sql-parsed .getReplaceExpressions seq)))
  (swap! context conj
         (let [table (-> sql-parsed
                         .getTable
                         get-fully-qualified-name)]
           (keyword (str (name table) ".*"))))
  (when-let [except (-> sql-parsed .getExceptColumns seq)]
    (let [except-context (atom [])]
      (doseq [x except]
        (.accept x that except-context))
      (swap! context (poke (fn [x] [x :except @except-context]))))))

(defmethod visit-after AllValue [_ _ context _]
  (swap! context conj :ALL))

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

(defmethod visit-before ArrayExpression [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ArrayExpression [_ sql-parsed context subcontext]
  (assert (not (.getStartIndexExpression sql-parsed)))
  (assert (not (.getStopIndexExpression sql-parsed)))
  (let [[object index] @subcontext]
    (swap! context conj
           (with-meta
             ;; The Honey SQL version, i.e. get-in, is variadic, which
             ;; is why we're using a subcontext. However, the Java SQL
             ;; Parser version seems to be different, e.g. support for
             ;; a range syntax
             [:get-in object index]
             ;; This makes for an easy test in visit-after SelectItem
             {:type :sql-fn}))))

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

(defmethod visit-after BooleanValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

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
  (assert (nil? (.getCommentText sql-parsed)))
  (swap! context conj (get-fully-qualified-name sql-parsed)))

(defmethod visit-after DoubleValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

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

(defmethod visit-after IsBooleanExpression [_ sql-parsed context _]
  (swap! context
         (poke (fn [left]
                 (let [op    (if (.isNot sql-parsed) :is-not :is)
                       right (if (.isTrue sql-parsed) true false)]
                   [op left right])))))

(defmethod visit-after InExpression [_ sql-parsed context _]
    (assert (not (.isGlobal sql-parsed)))
    (swap! context
           (fn [context']
             (let [op       (if (.isNot sql-parsed) :not-in :in)
                   right    (peek context')
                   context' (pop context')
                   left     (peek context')
                   context' (pop context')]
               (conj context' [op left right])))))

(defmethod visit-after IsNullExpression [_ sql-parsed context _]
  (swap! context (poke #(let [op (if (.isNot sql-parsed) :<> :=)]
                          [op % nil]))))

(defmethod visit-after LongValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

(defmethod visit-after NullValue [_ _ context _]
  (swap! context conj nil))

(defmethod visit-before ParenthesedExpressionList [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ParenthesedExpressionList [_ _ context subcontext]
  (swap! context conj @subcontext))

(defmethod visit-before ParenthesedSelect [that sql-parsed context]
  (assert (not (.getPivot sql-parsed)))
  (.accept (.getSelect sql-parsed) that context)
  ;; skip ExpressionVisitorAdapter/.visit and visit-after
  [nil nil])

(defmethod visit-after Select [_ _ _ _])

(defmethod visit-after SignedExpression [_ _ context _]
  (swap! context (poke #(- %))))

(defmethod visit-after StringValue [_ sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

(defmethod visit-after TimeKeyExpression [_ sql-parsed context _]
  (swap! context conj
         (condp = (.getStringValue sql-parsed)
           "current_date"        :current_date
           "current_date()"      :current_date
           "current_timestamp"   :current_timestamp
           "current_timestamp()" :current_timestamp
           (throw
            (ex-info "unknown value"
                     {:string-value (.getStringValue sql-parsed)
                      :type         (type sql-parsed)
                      :sql-parsed   sql-parsed
                      :context      context})))))

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
  (swap! context (poke #(sqh/order-by (if (= (type %) :sql-fn)
                                        [%]
                                        %)))))

(defmethod visit-order-by java.util.List [that sql-parsed context]
    (let [subcontext (atom [])]
      (doseq [x sql-parsed]
        (visit-order-by that x subcontext))
      (swap! context conj (apply merge-with into @subcontext))))
