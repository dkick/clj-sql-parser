(ns dkick.clj-sql-parser.statement.select
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select
    PlainSelect SelectVisitorAdapter)))

(defmulti visit-after multifn/visit-context-group)

(defmethod visit-after PlainSelect [_ context]
  #t _
  #t (.getFromItem _)
  (swap! context (fn [x] [(apply merge-with into x)])))

(defn select-visitor
  [expression-visitor pivot-visitor select-item-visitor from-item-visitor]
  (proxy [SelectVisitorAdapter]
      [expression-visitor pivot-visitor
       select-item-visitor from-item-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (visit-after sql-parsed context))
      context)))
