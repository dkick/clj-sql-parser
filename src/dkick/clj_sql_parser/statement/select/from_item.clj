(ns dkick.clj-sql-parser.statement.select.from-item
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]
   [dkick.clj-sql-parser.olio :refer [iff-first]]
   [honey.sql.helpers :as sqh])
  (:import
   (net.sf.jsqlparser.schema Table)
   (net.sf.jsqlparser.statement.select
    FromItemVisitorAdapter ParenthesedSelect)))

(defmulti visit-before multifn/visit-context-group)
(defmulti visit-after multifn/visit-subcontext-group)

(defmethod visit-before Object [_ context]
  context)

(defmethod visit-before ParenthesedSelect [_ _]
  (atom []))

(defmethod visit-after ParenthesedSelect [_ context subcontext]
  (swap! context conj
         (sqh/from (apply merge-with into @subcontext))))

(defmethod visit-after Table [sql-parsed context _]
  (let [x (.getFullyQualifiedName sql-parsed)]
    (assert (= x (.toString sql-parsed)))
    (swap! context conj #t (-> x keyword sqh/from))
    @context))

(defn from-item-visitor [expression-visitor]
  (proxy [FromItemVisitorAdapter] [expression-visitor]
    (visit [sql-parsed context]
      (when sql-parsed
        (let [subcontext (visit-before sql-parsed context)]
          (proxy-super visit sql-parsed subcontext)
          (visit-after sql-parsed context subcontext)))
      context)))
