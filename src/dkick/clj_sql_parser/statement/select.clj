(ns dkick.clj-sql-parser.statement.select
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select
    PlainSelect SelectVisitorAdapter)))

(defmulti -visit multifn/visit-group)

(defmethod -visit PlainSelect [_ context]
  (swap! context (fn [$] [(apply sqh/select $)])))

(defn select-visitor
  [expression-visitor pivot-visitor select-item-visitor from-item-visitor]
  (proxy [SelectVisitorAdapter]
      [expression-visitor pivot-visitor
       select-item-visitor from-item-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
