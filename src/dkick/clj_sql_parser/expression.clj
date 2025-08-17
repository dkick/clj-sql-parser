(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.expression Function LongValue)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select AllColumns)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context]
  context)

(defmethod visit-after AllColumns [sql-parsed context _]
  (assert (-> sql-parsed .getExceptColumns seq nil?))
  (assert (-> sql-parsed .getReplaceExpressions seq nil?))
  (swap! context conj :*))

(defmethod visit-after Column [sql-parsed context _]
  (swap! context conj (-> sql-parsed .getFullyQualifiedName keyword)))

(defmethod visit-before Function [_ _] (atom []))

(defmethod visit-after Function [sql-parsed context subcontext]
  (swap! context conj
         [(apply conj [(-> sql-parsed .getName keyword)] @subcontext)]))

(defmethod visit-after LongValue [sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

