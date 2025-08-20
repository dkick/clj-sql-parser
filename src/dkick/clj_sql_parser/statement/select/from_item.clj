(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select Join ParenthesedSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-before ParenthesedSelect [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ParenthesedSelect [_ sql-parsed context subcontext]
  (swap! context conj
         (let [alias (some-> sql-parsed .getAlias .getName keyword)]
           (sqh/from (cond-> [(apply merge-with into @subcontext)]
                       alias (conj alias))))))

(defmethod visit-after Table [_ sql-parsed context _]
  (let [x-name (keyword (.getFullyQualifiedName sql-parsed))
        alias  (.getAlias sql-parsed)]
    (swap! context conj
           (sqh/from (if-not alias
                       x-name
                       [x-name (-> alias .getName keyword)])))))

(defn join-data [x]
  (sort
   {:on-expressions (.getOnExpressions x)
    :using-columns  (.getUsingColumns x)
    :outer          (.isOuter x)
    :right          (.isRight x)
    :left           (.isLeft x)
    :natural        (.isNatural x)
    :global         (.isGlobal x)
    :full           (.isFull x)
    :inner          (.isInner x)
    :simple         (.isSimple x)
    :cross          (.isCross x)
    :semi           (.isSemi x)
    :straight       (.isStraight x)
    :apply          (.isApply x)
    :fromItem       (.getFromItem x)}))

(defmulti visit-join
  (fn [_from-visitor sql-parsed]
    (type sql-parsed)))

(defmethod visit-join Object [that sql-parsed]
  (let [context (atom [])]
    (.accept sql-parsed that context)
    @context))

(defmethod visit-join ParenthesedSelect [that sql-parsed]
  (let [context (atom [])
        alias   (some-> sql-parsed .getAlias .getName keyword)]
    (.accept (.getSelect sql-parsed) (.getSelectVisitor that) context)
    (if-not alias
      @context
      [(conj @context alias)])))

(defmethod visit-join Table [_ sql-parsed]
  (let [x-name (keyword (.getFullyQualifiedName sql-parsed))
        alias  (.getAlias sql-parsed)]
    [(if-not alias
       x-name
       [x-name (-> alias .getName keyword)])]))

(defmethod visit-join Join [that sql-parsed]
  (let [join-type
        (cond
          (.isOuter sql-parsed) :outer
          (.isRight sql-parsed) :right
          (.isFull sql-parsed)  :full
          (.isLeft sql-parsed)  :left
          (.isInner sql-parsed) :inner

          (not-any? #(% sql-parsed)
                    [Join/.isApply
                     Join/.isCross
                     Join/.isFull
                     Join/.isGlobal
                     Join/.isInner
                     Join/.isLeft
                     Join/.isNatural
                     Join/.isOuter
                     Join/.isRight
                     Join/.isSemi
                     Join/.isSimple
                     Join/.isStraight])
          :join

          :else
          (throw
           (ex-info
            "Unknown type of join"
            {:join-object sql-parsed
             :join-data   (join-data sql-parsed)})))

        table (visit-join that (.getFromItem sql-parsed))

        table
        (if-not (seq (.getUsingColumns sql-parsed))
          table
          (let [context (atom [])]
            (doseq [x (.getUsingColumns sql-parsed)]
              (.accept x (.getExpressionVisitor that) context))
            (conj table `[:using ~@(deref context)])))

        table
        (if-not (seq (.getOnExpressions sql-parsed))
          table
          (let [context (atom [])]
            (doseq [x (.getOnExpressions sql-parsed)]
              (.accept x (.getExpressionVisitor that) context))
            (apply conj table @context)))]

    [join-type table]))

(defn visit-joins [that joins context]
  (doseq [[x-joins y-joins]
          (->> (partition-by #(.isSimple %) joins)
               (partition 2 1 nil))]
    (assert (or (nil? y-joins) (not (.isSimple (first y-joins)))))
    (if (.isSimple (first x-joins))
      (doseq [x x-joins]
        (-> x .getFromItem (.accept that context)))
      (->> x-joins
           (mapcat #(visit-join that %))
           (apply sqh/join-by)
           (swap! context conj)))))
