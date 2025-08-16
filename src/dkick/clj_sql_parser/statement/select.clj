(ns dkick.clj-sql-parser.statement.select
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select PlainSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context]
  context)

(defmethod visit-after PlainSelect [_ context _]
  (swap! context (fn [x] [(apply merge-with into x)])))

#_(defn select-visitor
  [expression-visitor pivot-visitor select-item-visitor from-item-visitor]
  (proxy [SelectVisitorAdapter]
      [expression-visitor pivot-visitor
       select-item-visitor from-item-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (visit-after sql-parsed context))
      context)))
