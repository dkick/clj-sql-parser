(ns dkick.clj-sql-parser.statement.select.from-item.joins
  (:require
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select Join ParenthesedSelect)))

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

(defn join-data [x]
  (sort
   {:on-expressions (.getOnExpressions x)
    :using-columns  (.getUsingColumns x)

    :outer    (.isOuter x)    :right   (.isRight x),
    :left     (.isLeft x)     :natural (.isNatural x)
    :global   (.isGlobal x)   :full    (.isFull x)
    :inner    (.isInner x)    :simple  (.isSimple x)
    :cross    (.isCross x)    :semi    (.isSemi x)
    :straight (.isStraight x) :apply   (.isApply x)
    :fromItem (.getFromItem x)}))

(def types-of-join
  [Join/.isApply Join/.isCross Join/.isFull    Join/.isGlobal
   Join/.isInner Join/.isLeft  Join/.isNatural Join/.isOuter
   Join/.isRight Join/.isSemi  Join/.isSimple  Join/.isStraight])

(defn default? [sql-parsed]
  (not-any? #(% sql-parsed) types-of-join))

(defn type-of-join [sql-parsed]
  ;; We were a bit confused by the logic in the Join/.toString for how
  ;; to check these flags, and how those checks interact with the
  ;; HoneySQL types. This is a best guess.
  (cond (.isOuter sql-parsed) :outer
        (.isRight sql-parsed) :right
        (.isFull sql-parsed)  :full
        (.isLeft sql-parsed)  :left
        (.isInner sql-parsed) :inner
        (default? sql-parsed) :join

        :else
        (throw
         (ex-info
          "Unknown type of join"
          {:join-object sql-parsed
           :join-data   (join-data sql-parsed)}))))

(defmethod visit-join Join [that sql-parsed]
  (let [what-to-join (visit-join that (.getFromItem sql-parsed))

        what-to-join
        (if-not (seq (.getUsingColumns sql-parsed))
          what-to-join
          (let [context (atom [])]
            (doseq [x (.getUsingColumns sql-parsed)]
              (.accept x (.getExpressionVisitor that) context))
            (conj what-to-join `[:using ~@(deref context)])))

        what-to-join
        (if-not (seq (.getOnExpressions sql-parsed))
          what-to-join
          (let [context (atom [])]
            (doseq [x (.getOnExpressions sql-parsed)]
              (.accept x (.getExpressionVisitor that) context))
            (apply conj what-to-join @context)))]

    [(type-of-join sql-parsed) what-to-join]))

(defn visit-joins [that joins context]
  (doseq [[x-joins y-joins]
          (->> (partition-by Join/.isSimple joins)
               (partition 2 1 nil))]
    (assert (or (nil? y-joins) (not (.isSimple (first y-joins)))))
    (if (.isSimple (first x-joins))
      (doseq [x x-joins]
        (-> x .getFromItem (.accept that context)))
      (->> x-joins
           (mapcat #(visit-join that %))
           (apply sqh/join-by)
           (swap! context conj)))))
