(ns dkick.clj-sql-parser.statement.select.select-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [poke]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select
    SelectItem SelectItemVisitorAdapter)))

(defmulti -visit multifn/visit-context-group)

(defmethod -visit SelectItem [_ context]
  (swap! context (poke sqh/select)))

(defn select-item-visitor [expression-visitor]
  (proxy [SelectItemVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
