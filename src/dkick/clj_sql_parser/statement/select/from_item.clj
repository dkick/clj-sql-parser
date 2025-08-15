(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select FromItemVisitorAdapter)))

(defmulti -visit multifn/visit-group)

(defmethod -visit Table [sql-parsed context]
  (let [x (.getFullyQualifiedName sql-parsed)]
    (assert (= x (.toString sql-parsed)))
    (swap! context conj (-> x keyword sqh/from))))

(defn from-item-visitor [expression-visitor]
  (proxy [FromItemVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
