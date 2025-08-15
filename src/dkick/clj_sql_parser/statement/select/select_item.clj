(ns dkick.clj-sql-parser.statement.select.select-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select SelectItem SelectItemVisitorAdapter)))

(defmulti -visit multifn/visit-group)

(defmethod -visit SelectItem [_ _])

(defn select-item-visitor [expression-visitor]
  (proxy [SelectItemVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      context)))
