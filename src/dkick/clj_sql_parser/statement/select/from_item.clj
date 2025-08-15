(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select FromItemVisitorAdapter)))

(defmulti -visit multifn/visit-group)

(defn from-item-visitor [expression-visitor]
  (proxy [FromItemVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
