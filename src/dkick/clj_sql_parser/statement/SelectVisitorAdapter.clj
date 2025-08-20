(ns dkick.clj-sql-parser.statement.SelectVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select
    :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.select.SelectVisitorAdapter
   :exposes-methods {visit superVisit}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [[sql-parsed subcontext]
          (visit-before this sql-parsed context)]
      (.superVisit this sql-parsed subcontext)
      (visit-after this sql-parsed context subcontext)))
  context)
