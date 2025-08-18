(ns dkick.clj-sql-parser.statement.select
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select ParenthesedSelect PlainSelect)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-before ParenthesedSelect [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ParenthesedSelect [_ _ context subcontext]
  (swap! context conj (sqh/select [(apply merge-with into @subcontext)])))

;; The SelectVisitorAdapter visits the where clause as an Expression,
;; i.e. there is no derived type to distinguish it as a where clause,
;; which makes it rather hard to use for our purposes. Here we have to
;; do work around this limitation
(defmethod visit-before PlainSelect [_ sql-parsed context]
  ;; Remember the original sql-parsed clauses to be visited in the
  ;; vist-after PlainSelect
  (swap! context conj {:where   (.getWhere sql-parsed)
                       :having  (.getHaving sql-parsed)
                       :qualify (.getQualify sql-parsed)
                       :offset  (.getOffset sql-parsed)
                       :fetch   (.getFetch sql-parsed)})
  ;; TODO: At the moment, we do not handle the following
  (assert (nil? (:qualify (peek @context))))
  (assert (nil? (:offset (peek @context))))
  (assert (nil? (:fetch (peek @context))))
  ;; Modify sql-parsed to remove the problematic parts. We will
  ;; visit-after PlainSelect them later
  [(doto sql-parsed
     (.setWhere nil)
     (.setHaving nil)
     (.setQualify nil)
     (.setOffset nil)
     (.setFetch nil))
   ;; And have the superclass visit use a subcontext in order to not
   ;; push anything on top of the original sql-parsed parts
   (atom [])])

(defmethod visit-after PlainSelect [that _ context subcontext]
  (peek @context)
  (let [sql-parts          (peek @context)
        expression-visitor (.getExpressionVisitor that)]
    (swap! context pop)
    (when-let [where-sql (:where sql-parts)]
      (let [where-context (atom [])]
        (.accept where-sql expression-visitor where-context)
        (assert (= (count @where-context) 1))
        (swap! subcontext conj (sqh/where (peek @where-context)))))
    (when-let [having-sql (:having sql-parts)]
      (let [having-context (atom [])]
        (.accept having-sql expression-visitor having-context)
        (assert (= (count @having-context) 1))
        (swap! subcontext conj (sqh/having (peek @having-context)))))
    (swap! subcontext (fn [x] [(apply merge-with into x)]))
    (assert (= (count @subcontext) 1))
    (swap! context #(apply conj %1 %2) @subcontext)))
