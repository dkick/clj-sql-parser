(ns dkick.clj-sql-parser.statement.select.PivotVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select.pivot
    :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.select.PivotVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [subcontext
          (visit-before this sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after this sql-parsed context subcontext)))
  context)
