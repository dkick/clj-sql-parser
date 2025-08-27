(ns dkick.clj-sql-parser.statement
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select PlainSelect SetOperationList)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-after PlainSelect [_ _ _ _])

(defmethod visit-after SetOperationList [_ _ _ _])
