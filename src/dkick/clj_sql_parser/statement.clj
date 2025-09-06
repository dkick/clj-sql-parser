(ns dkick.clj-sql-parser.statement
  (:require
   [dkick.clj-sql-parser.visitors :as visitors])
  (:import
   (net.sf.jsqlparser.statement.create.view
    CreateView)
   (net.sf.jsqlparser.statement.select
    ParenthesedSelect PlainSelect SetOperationList)))

(defmulti visit-after visitors/visit-after-group)
(defmulti visit-before visitors/visit-before-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-after CreateView [_ sql-parsed context subcontext]
  (throw (ex-info "N/A" {:sql-parsed sql-parsed
                         :context    context
                         :subcontext subcontext})))

(defmethod visit-after ParenthesedSelect [_ _ _ _])

(defmethod visit-after PlainSelect [_ _ _ _])

(defmethod visit-after SetOperationList [_ _ _ _])
