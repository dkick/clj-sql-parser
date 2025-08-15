(ns dkick.clj-sql-parser.statement
  (:require
   [dkick.clj-sql-parser.expression :refer [expression-visitor]]
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.statement.select.pivot :refer [pivot-visitor]]
   [dkick.clj-sql-parser.statement.select :refer [select-visitor]]
   [dkick.clj-sql-parser.statement.select.from-item
    :refer [from-item-visitor]]
   [dkick.clj-sql-parser.statement.select.select-item
    :refer [select-item-visitor]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement StatementVisitorAdapter)
   (net.sf.jsqlparser.statement.select PlainSelect)))

(defmulti -visit multifn/visit-group)

(defmethod -visit PlainSelect [_ _])

(defn visitors []
  (let [ev  (expression-visitor)
        pv  (pivot-visitor ev)
        siv (select-item-visitor ev)
        fiv (from-item-visitor ev)
        sv  (select-visitor ev pv siv fiv)]
    {:expression-visitor  (doto ev (.setSelectVisitor sv))
     :from-item-visitor   (doto fiv (.setSelectVisitor sv))
     :pivot-visitor       pv
     :select-item-visitor siv
     :select-visitor      sv}))

(defn statement-visitor
  ([]
   (let [{:keys [select-visitor]}
         (visitors)]
     (statement-visitor select-visitor)))
  ([select-visitor]
   (proxy [StatementVisitorAdapter] [select-visitor]
     (visit [sql-parsed context]
       (when sql-parsed
         (proxy-super visit sql-parsed context)
         (-visit sql-parsed context))
       (let [[car cdr] @context]
         (assert (nil? cdr))
         car)))))

(comment
  (visitors)
  #_|)
