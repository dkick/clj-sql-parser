(ns dkick.clj-sql-parser.statement.select.pivot
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select PivotVisitorAdapter)))

(defmulti -visit multifn/visit-context-group)

(defn pivot-visitor [expression-visitor]
  (proxy [PivotVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
