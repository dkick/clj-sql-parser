(ns dkick.clj-sql-parser.statement.SelectVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select
    :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.select.SelectVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [subcontext (visit-before sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after sql-parsed context subcontext)))
  context)
