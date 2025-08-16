(ns dkick.clj-sql-parser.StatementVisitorAdapter
  (:require
   [dkick.clj-sql-parser.statement :refer [visit-after visit-before]])
  (:gen-class
   :extends net.sf.jsqlparser.statement.StatementVisitorAdapter
   :exposes-methods {visit visitSuper}))

(defn -visit [this sql-parsed context]
  (when sql-parsed
    (let [subcontext (visit-before sql-parsed context)]
      (.visitSuper this sql-parsed subcontext)
      (visit-after sql-parsed context subcontext)))
  (let [[car & cdr] @context]
    (assert (nil? cdr))
    car))
