(ns dkick.clj-sql-parser.select
  (:require
   [dkick.clj-sql-parser.expression :refer [expression-visitor]]
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select
    PlainSelect SelectVisitorAdapter)))

(defmulti -visit multifn/visit-group)

(defmethod -visit PlainSelect [_ context]
  (swap! context (fn [$] [(apply sqh/select $)])))

#_(defmethod -visit PlainSelect [_ _])

(def select-visitor
  (proxy [SelectVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
