(ns dkick.clj-sql-parser.statement.table
  (:require
   [dkick.clj-sql-parser.schema :refer [get-fully-qualified-name]]
   [honey.sql :as sql]
   [honey.sql.helpers :as sqh]))

(defn create-or-replace-table-formatter [_ x]
  (#'sql/format-create :create-or-replace :table x nil))

(sql/register-clause!
 :create-or-replace-table
 create-or-replace-table-formatter
 :create-table)

(defn create-or-replace-table [& args]
  (sqh/generic-helper-variadic :create-or-replace-table args))

(defn create-table-fn [sql-parsed]
  {:pre [(not (.isUnlogged sql-parsed))
         ;; We have examples with these but we have no idea what to do
         ;; with them
         #_(empty? (.getTableOptionsStrings sql-parsed))]}
  (let [replace? #(.isOrReplace sql-parsed)]
    (cond
      (replace?) create-or-replace-table
      :else      sqh/create-table)))

(def normalize-table
  #(if (= (count %) 1)
     (first %)
     %))

(defn make-table [sql-parsed]
  (let [fqn  (-> sql-parsed .getTable get-fully-qualified-name)
        ine? #(.isIfNotExists sql-parsed)]
    (normalize-table
     (cond-> []
      fqn    (conj fqn)
      (ine?) (conj :if-not-exists)))))

(defn make-with-columns [sql-parsed]
  (->> (.getColumnDefinitions sql-parsed)
       (map (fn [x]
              [(-> x .getColumnName keyword)
               (-> x .getColDataType .getDataType keyword)]))
       (reduce #(-> %1 (sqh/with-columns %2)) {})))

(defn create-table [_that sql-parsed context]
  {:pre [(empty? (.getColumns sql-parsed))]}
  (let [create  (create-table-fn sql-parsed)
        table   (make-table sql-parsed)
        columns (make-with-columns sql-parsed)]
    (swap! context conj (merge (create table) columns))))

(comment
  (-> (create-or-replace-table :foo)
      (sql/format {:inline true}))
  #__)
