(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.visitors :as visitors]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select ParenthesedSelect)))

(defmulti visit-after visitors/visit-after-group)
(defmulti visit-before visitors/visit-before-group)

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
