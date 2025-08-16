(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select ParenthesedSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context]
  context)

(defmethod visit-before ParenthesedSelect [_ _]
  (atom []))

(defmethod visit-after ParenthesedSelect [_ context subcontext]
  (swap! context conj
         (sqh/from [(apply merge-with into @subcontext)])))

(defmethod visit-after Table [sql-parsed context _]
  (let [x (.getFullyQualifiedName sql-parsed)]
    (assert (= x (.toString sql-parsed)))
    (swap! context conj (-> x keyword sqh/from))))
