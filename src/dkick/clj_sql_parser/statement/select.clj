(ns dkick.clj-sql-parser.statement.select
  (:require
   [clojure.set :as set]
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [iff-first poke]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.statement.select
    ParenthesedSelect PlainSelect WithItem)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

(defmethod visit-before ParenthesedSelect [_ sql-parsed _]
  [sql-parsed (atom [])])

(defmethod visit-after ParenthesedSelect [_ _ context subcontext]
  (swap! context conj [(apply merge-with into @subcontext)]))

;; There are some issues and/or bugs in the members of a
;; PlainSelect. We work around these here by modifing the instance in
;; visit-before before passing it into the base visitor
;;
;; In general, we remember the original sql-parsed clauses to be
;; visited in the vist-after PlainSelect in the original context, and
;; pass a new subcontext into the superclass visitor
(defmethod visit-before PlainSelect [_ sql-parsed context]
  ;; The SelectVisitorAdapter visits some of these clause as an
  ;; Expression, i.e. there is no derived type to distinguish it from
  ;; other types of expressions, which makes it rather hard to use for
  ;; our purposes
  (swap! context conj {:where   (.getWhere sql-parsed)
                       :having  (.getHaving sql-parsed)
                       :qualify (.getQualify sql-parsed)
                       :offset  (.getOffset sql-parsed)
                       :fetch   (.getFetch sql-parsed)})
  ;; TODO: At the moment, we do not handle the following
  (assert (nil? (:offset (peek @context))))
  (assert (nil? (:fetch (peek @context))))

  ;; The SelectVisitorAdapter superclass will
  ;; try to process
  ;;     (.. sql-parsed getDistinct getOnSelectItems)
  ;; without checking for a null value
  (swap! context
         (poke #(merge % {:distinct (.getDistinct sql-parsed)})))

  [;; Modify sql-parsed to remove the problematic parts. We will
   ;; visit-after PlainSelect them later
   (doto sql-parsed
     ;; Expressions without distinct types
     (.setWhere nil)
     (.setHaving nil)
     (.setQualify nil)
     (.setOffset nil)
     (.setFetch nil)
     ;; Distinct with null getOnSelectItems
     (.setDistinct nil))
   (atom [])])

(defmethod visit-after PlainSelect [that _ context subcontext]
  (let [sql-parts-delayed  (peek @context)
        expression-visitor (.getExpressionVisitor that)]
    (swap! context pop)
    (when-let [where-sql (:where sql-parts-delayed)]
      (let [where-context (atom [])]
        (.accept where-sql expression-visitor where-context)
        (assert (= (count @where-context) 1))
        (swap! subcontext conj (sqh/where (peek @where-context)))))
    (when-let [having-sql (:having sql-parts-delayed)]
      (let [having-context (atom [])]
        (.accept having-sql expression-visitor having-context)
        (assert (= (count @having-context) 1))
        (swap! subcontext conj (sqh/having (peek @having-context)))))
    (when-let [qualify-sql (:qualify sql-parts-delayed)]
      (let [qualify-context (atom [])]
        (.accept qualify-sql expression-visitor qualify-context)
        (assert (= (count @qualify-context) 1))
        (swap! subcontext conj `{:qualify ~@(deref qualify-context)})))
    (swap! subcontext
           (fn [x]
             [(cond-> (apply merge-with into x)
                (:distinct sql-parts-delayed)
                (set/rename-keys {:select :select-distinct}))]))
    (assert (= (count @subcontext) 1))
    (swap! context #(apply conj %1 %2) @subcontext)))

(defmethod visit-after WithItem [_ sql-parsed context _]
  (let [alias (some-> sql-parsed .getAlias .getName keyword)]
    (assert alias)
    (swap! context (poke #(sqh/with [alias (iff-first %)])))))
