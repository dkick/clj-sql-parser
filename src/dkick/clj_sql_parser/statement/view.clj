(ns dkick.clj-sql-parser.statement.view
  (:require
   [clojure.string :as str]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema MultiPartName)
   (net.sf.jsqlparser.statement.create.view
    AutoRefreshOption ForceOption TemporaryOption)))

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

(defn make-view [sql-parsed]
  ;; TODO We might need to do this split, unquote, join with fully
  ;; qualified names in other places
  (let [view (-> sql-parsed .getView .getFullyQualifiedName)
        view (->> (str/split view #"\.")
                  (map MultiPartName/unquote)
                  (str/join "."))
        view (keyword view)

        expr
        (cond-> []
          (not= (.getTemporary sql-parsed) TemporaryOption/NONE)
          (conj (-> sql-parsed .getTemporary .getName keyword))

          view (conj view)

          (.getViewCommentOptions sql-parsed)
          (conj (let [[wtf <<comment>> comment]
                      (.getViewCommentOptions sql-parsed)]
                  (assert (string? wtf))
                  (assert (empty? wtf)) ;why?
                  (assert (= "comment" <<comment>>))
                  (assert (string? (not-empty comment)))
                  [:comment
                   (let [n (-> comment count dec)]
                     (-> comment (subs 1 n)))]))

          (.isIfNotExists sql-parsed)
          (conj :if-not-exists))]

    expr
    #_(condp = (count expr)
      0 (throw (ex-info "empty view expression"
                        {:sql-parsed sql-parsed
                         :view       view
                         :expr       expr}))
      1 (let [[view] expr]
          view)
      expr)))

(defn make-with-columns [sql-parsed]
  (->> (.getColumnNames sql-parsed)
       ;; In a create-table, we would need the data types to go with
       ;; the column names. However, this is a Databricks specific
       ;; variation for create-view; these column names are almost
       ;; more like aliases that get applied to the AS SELECT for the
       ;; CREATE VIEW, but the name must always be in a sequence
       ;; because the Honey SQL format will be expecting more than
       ;; just a name
       (map #(let [s (.getCommentText %)
                   n (some-> s count dec)
                   s (some-> s (subs 1 n))]
               (cond-> [(-> % .getUnquotedName keyword)]
                 s (conj [:comment s]))))
       (reduce #(-> %1 (sqh/with-columns %2)) {})))

(defn make-select [that sql-parsed]
  (let [context (atom [])]
    (.accept (.getSelect sql-parsed) that context)))

(defn create-view [that sql-parsed context]
  ;; The StatementVisitorAdapter doesn't do much for this kind of
  ;; statement. We have to implement a lot of this logic ourselves
  (assert (= (.getForce sql-parsed) ForceOption/NONE))
  (assert (not (.isSecure sql-parsed)))
  (assert (= (.getAutoRefresh sql-parsed) AutoRefreshOption/NONE))
  (assert (not (.isWithReadOnly sql-parsed)))
  (let [create   (create-view-fn sql-parsed)
        view     (make-view sql-parsed)
        columns  (make-with-columns sql-parsed)
        [select] (make-select that sql-parsed)]

    (swap! context conj (merge (apply create view)
                               columns
                               select))))
