(ns dkick.clj-sql-parser.statement.select.FromItemVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select.from-item
    :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.select.FromItemVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [[sql-parsed subcontext]
          (visit-before this sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after this sql-parsed context subcontext)))
  context)
