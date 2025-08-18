(ns dkick.clj-sql-parser.statement
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.statement.select PlainSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ _ context]
  context)

(defmethod visit-after PlainSelect [_ _ _ _])
