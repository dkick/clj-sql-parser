(ns dkick.clj-sql-parser.statement
  (:require
   [dkick.clj-sql-parser.visitors :as visitors]
   [dkick.clj-sql-parser.statement.table :refer [create-table]]
   [dkick.clj-sql-parser.statement.view :refer [create-view]])
  (:import
   (net.sf.jsqlparser.statement.create.table CreateTable)
   (net.sf.jsqlparser.statement.create.view CreateView)
   (net.sf.jsqlparser.statement.select
    ParenthesedSelect PlainSelect SetOperationList)))

(defmulti visit-after visitors/visit-after-group)
(defmulti visit-before visitors/visit-before-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-before CreateTable [that sql-parsed context]
  (create-table that sql-parsed context)
  ;; We avoid the superclass method because it ends up passing the
  ;; table into a fromItemVisitor, and we don't want a FROM clause
  ;; here
  [nil nil])

(defmethod visit-after CreateView [that sql-parsed context _]
  (create-view that sql-parsed context))

(defmethod visit-after ParenthesedSelect [_ _ _ _])

(defmethod visit-after PlainSelect [_ _ _ _])

(defmethod visit-after SetOperationList [_ _ _ _])
