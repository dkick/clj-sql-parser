(ns dkick.clj-sql-parser.statement.select.select-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [poke]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select ParenthesedSelect SelectItem)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defn -sql-fn? [x]
  (= (type x) :sql-fn))

(defmethod visit-before ParenthesedSelect [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ParenthesedSelect [_ _ context subcontext]
  (swap! context conj (sqh/select [(apply merge-with into @subcontext)])))

(defmethod visit-after SelectItem [_ sql-parsed context _]
  (swap! context
         (poke #(let [-fn?  (-sql-fn? %)
                      alias (some-> sql-parsed .getAliasName keyword)]
                  (sqh/select
                   (cond
                     alias [% alias]    ; also SQL fn w/ alias
                     -fn?  [%]          ; only SQL fn w/o alias
                     :else %))))))
