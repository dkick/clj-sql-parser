(ns dkick.clj-sql-parser.statement.select.select-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [poke simple?]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select SelectItem)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context]
  context)

(defmethod visit-after SelectItem [sql-parsed context _]
  (swap! context
         (poke #(let [alias (.getAliasName sql-parsed)]
                  (sqh/select
                   (cond
                     (nil? alias) %
                     (vector? %)  (conj % (keyword alias))
                     (simple? %)  [% (keyword alias)]
                     :else        (throw
                                   (ex-info
                                    "Unknown SeleteItem context"
                                    {:%          %
                                     :alias      alias
                                     :sql-parsed sql-parsed
                                     :context    context}))))))))
