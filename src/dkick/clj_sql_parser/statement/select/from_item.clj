(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select ParenthesedSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ _ context] context)

(defmethod visit-before ParenthesedSelect [_ _ _] (atom []))

(defmethod visit-after ParenthesedSelect [_ _ context subcontext]
  (swap! context conj
         (sqh/from [(apply merge-with into @subcontext)])))

(defmethod visit-after Table [_ sql-parsed context _]
  (let [-name (keyword (.getFullyQualifiedName sql-parsed))
        alias (.getAlias sql-parsed)]
    (swap! context conj
           (sqh/from (if-not alias
                       -name
                       [-name (-> alias .getName keyword)])))))
