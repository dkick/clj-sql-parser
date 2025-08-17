(ns dkick.clj-sql-parser.statement.select.select-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [poke simple?]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select SelectItem)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context] context)

(defn -sql-fn? [x]
  (= (type x) :sql-fn))

(defmethod visit-after SelectItem [sql-parsed context _]
  (swap! context
         (poke #(let [alias (.getAliasName sql-parsed)
                      alias (when alias (keyword alias))]
                  (sqh/select
                   (cond
                     alias        [% alias]
                     (-sql-fn? %) [%]
                     :else        %))))))
