(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select ParenthesedSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-before ParenthesedSelect [_ sql-parsed _]
  [sql-parsed (atom [])])

(def ^:dynamic *visiting-legit-joins*
  "We don't like using a dynamic variable here but, at the moment, it
  seems the easiest way to be sure we don't assume a ParenthesedSelect
  is associated with a FromItem when it might be found in a join"
  false)

(defmethod visit-after ParenthesedSelect [_ sql-parsed context subcontext]
  (assert (not *visiting-legit-joins*))
  (swap! context conj
         (let [alias (some-> sql-parsed .getAlias .getName keyword)]
           (sqh/from (cond-> [(apply merge-with into @subcontext)]
                       alias (conj alias))))))

(defmethod visit-after Table [_ sql-parsed context _]
  (let [-name (keyword (.getFullyQualifiedName sql-parsed))
        alias (.getAlias sql-parsed)]
    (swap! context conj
           (sqh/from (if-not alias
                       -name
                       [-name (-> alias .getName keyword)])))))

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

(defn visit-join [that join]
  (let [join-type
        (cond
          (.isOuter join) :outer
          (.isRight join) :right
          (.isFull join)  :full
          (.isLeft join)  :left
          (.isInner join) :inner

          :else
          (throw
           (ex-info
            "Unknown type of join"
            {:join-object join
             :join-data   (join-data join)})))

        table
        (let [from-item (.getFromItem join)
              -name     (keyword (.getFullyQualifiedName from-item))
              alias     (.getAlias from-item)]
          [(if-not alias
             -name
             [-name (-> alias .getName keyword)])])

        table
        (if-not (seq (.getUsingColumns join))
          table
          (let [context (atom [])]
            (doseq [x (.getUsingColumns join)]
              (.accept x (.getExpressionVisitor that) context))
            (conj table `[:using ~@(deref context)])))

        table
        (if-not (seq (.getOnExpressions join))
          table
          (let [context (atom [])]
            (doseq [x (.getOnExpressions join)]
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
      (binding [*visiting-legit-joins* true]
        (->> x-joins
             (mapcat #(visit-join that %))
             (apply sqh/join-by)
             (swap! context conj))))))
