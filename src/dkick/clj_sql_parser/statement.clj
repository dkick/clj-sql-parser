(ns dkick.clj-sql-parser.statement
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.select :refer [select-visitor]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement StatementVisitorAdapter)
   (net.sf.jsqlparser.statement.select PlainSelect)))

(defmulti -visit multifn/visit-group)

(defmethod -visit PlainSelect [_ _])

(def statement-visitor
  (proxy [StatementVisitorAdapter] [select-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (proxy-super visit sql-parsed context)
        (-visit sql-parsed context))
      (let [[car cdr] @context]
        (assert (nil? cdr))
        car))))
