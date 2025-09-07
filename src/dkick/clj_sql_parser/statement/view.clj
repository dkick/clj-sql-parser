(ns dkick.clj-sql-parser.statement.view
  (:require
   [dkick.clj-sql-parser.schema :refer [get-fully-qualified-name]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.create.view
    AutoRefreshOption CreateView ForceOption TemporaryOption)
   (net.sf.jsqlparser.statement.select
    ParenthesedSelect PlainSelect)))

(defn create-view-fn [sql-parsed]
  (cond
    (.isOrReplace sql-parsed)
    (do (assert (not (.isMaterialized sql-parsed)))
        sqh/create-or-replace-view)

    (.isMaterialized sql-parsed)
    (do (assert (not (.isOrReplace sql-parsed)))
        sqh/create-materialized-view)

    :else
    (do (assert (not (.isOrReplace sql-parsed)))
        (assert (not (.isMaterialized sql-parsed)))
        sqh/create-view)))

(defn make-with-columns [sql-parsed]
  (->> (.getColumnNames sql-parsed)
       ;; In a create-table, we would need the data types to go with
       ;; the column names. However, this is a Databricks specific
       ;; variation for create-view; these column names are almost
       ;; more like aliases that get applied to the AS SELECT for the
       ;; CREATE VIEW, but the name must always be in a sequence
       ;; because the Honey SQL format will be expecting more than
       ;; just a name
       (map (fn [x] [(-> x .getUnquotedName keyword)]))
       #_(map #(let [s (.getCommentText %)
                   n (some-> s count dec)
                   s (some-> s (subs 1 n))]
               (cond-> [(-> % .getUnquotedName keyword)]
                 s (conj [:comment s]))))
       (reduce #(-> %1 (sqh/with-columns %2)) {})))

(defn make-view [sql-parsed]
  (let [view (-> sql-parsed .getView get-fully-qualified-name)]
    (cond-> []
      (not= (.getTemporary sql-parsed) TemporaryOption/NONE)
      (conj (-> sql-parsed .getTemporary .getName keyword))

      view (conj view)

      (-> sql-parsed .getColumnNames seq)
      (conj (make-with-columns sql-parsed))

      #_(.getViewCommentOptions sql-parsed)
      #_(conj (let [[wtf <<comment>> comment]
                  (.getViewCommentOptions sql-parsed)]
              (assert (string? wtf))
              (assert (empty? wtf)) ;why?
              (assert (= "comment" <<comment>>))
              (assert (string? (not-empty comment)))
              [:comment (let [n (-> comment count dec)]
                          (-> comment (subs 1 n)))]))

      (.isIfNotExists sql-parsed)
      (conj :if-not-exists))))

(defmulti make-select
  (fn [_ sql-parsed]
    (type sql-parsed)))

(defmethod make-select CreateView [that sql-parsed]
  (make-select that (.getSelect sql-parsed)))

(defmethod make-select ParenthesedSelect [that sql-parsed]
  (make-select that (.getSelect sql-parsed)))

(defmethod make-select PlainSelect [that sql-parsed]
  (let [context (atom [])]
    (.accept sql-parsed that context)))

(defn create-view [that sql-parsed context]
  ;; The StatementVisitorAdapter doesn't do much for this kind of
  ;; statement. We have to implement a lot of this logic ourselves
  (assert (= (.getForce sql-parsed) ForceOption/NONE))
  (assert (not (.isSecure sql-parsed)))
  (assert (= (.getAutoRefresh sql-parsed) AutoRefreshOption/NONE))
  (assert (not (.isWithReadOnly sql-parsed)))
  (let [create (create-view-fn sql-parsed)
        view   (make-view sql-parsed)
        view   (apply create view)
        select (make-select that sql-parsed)]
    (swap! context conj (merge view select))))
