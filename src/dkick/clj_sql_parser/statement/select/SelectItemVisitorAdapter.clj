(ns dkick.clj-sql-parser.statement.select.SelectItemVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement.select.select-item
    :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.select.SelectItemVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [subcontext
          (visit-before this sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after this sql-parsed context subcontext)))
  context)

