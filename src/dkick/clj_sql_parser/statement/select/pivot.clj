(ns dkick.clj-sql-parser.statement.select.pivot
  (:require
   #_[dkick.clj-sql-parser.expression :refer [expression-visitor]]
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select PivotVisitorAdapter)))

(defmulti -visit multifn/visit-group)

(defn pivot-visitor [expression-visitor]
  (proxy [PivotVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
