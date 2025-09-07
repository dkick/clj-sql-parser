(ns dkick.clj-sql-parser.statement.select.from-item.joins
  (:require
   [dkick.clj-sql-parser.schema :refer [get-fully-qualified-name]]
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
  (let [x-name (get-fully-qualified-name sql-parsed)
        alias  (.getAlias sql-parsed)]
    [(if-not alias
       x-name
       [x-name (-> alias .getName keyword)])]))

(defn join-data [x]
  (sort
   {:on-expressions (.getOnExpressions x)
    :using-columns  (.getUsingColumns x)

    ;; JSQLParser has these as seperate booleans but they are not
    ;; really all independent values; e.g. an INNER OUTER join does
    ;; not make sense. The parser logic probably handles making sure
    ;; these values are coordinated correctly but at this level it can
    ;; be a bit hard tell at a glance, if one hasn't internalized them
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
  {:pre [;; We don't know what to do with GLOBAL
         (not (.isGlobal sql-parsed))
         ;; ... or NATURAL
         (not (.isNatural sql-parsed))
         ;; ... or SEMI
         (not (.isSemi sql-parsed))
         ;; ... or STRAIGHT_JOIN
         (not (.isStraight sql-parsed))
         ;; ... or APPLY
         (not (.isApply sql-parsed))]}
  (cond (.isRight sql-parsed) (do (assert (not (.isInner sql-parsed)))
                                  :right)
        (.isFull sql-parsed)  (do (assert (not (.isInner sql-parsed)))
                                  :full)
        (.isLeft sql-parsed)  (do (assert (not (.isInner sql-parsed)))
                                  :left)
        (.isCross sql-parsed) (do (assert (not (.isInner sql-parsed)))
                                  (assert (not (.isOuter sql-parsed)))
                                  :cross)

        (.isOuter sql-parsed) (do (assert (not (.isInner sql-parsed)))
                                  :outer)
        (.isInner sql-parsed) (do (assert (not (.isOuter sql-parsed)))
                                  :inner)

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

(defn simple-only? [x]
  (assert (not (.isGlobal x)))
  (and (.isSimple x)
       (not (.isOuter x))))

(defn visit-joins [that joins context]
  (doseq [[x-joins y-joins]
          (->> (partition-by simple-only? joins)
               (partition 2 1 nil))]
    (assert (or (nil? y-joins) (not (simple-only? (first y-joins)))))
    (if (simple-only? (first x-joins))
      (doseq [x x-joins]
        (-> x .getFromItem (.accept that context)))
      (->> x-joins
           (mapcat #(visit-join that %))
           (apply sqh/join-by)
           (swap! context conj)))))
