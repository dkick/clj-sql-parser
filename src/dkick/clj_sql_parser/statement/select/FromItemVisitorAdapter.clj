(ns dkick.clj-sql-parser.statement.select.FromItemVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select.from-item
    :refer [visit-after visit-before]])
  (:gen-class
   :exposes-methods {visit visitSuper}
   :extends net.sf.jsqlparser.statement.select.FromItemVisitorAdapter))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [subcontext (visit-before sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after sql-parsed context subcontext)))
  context)
