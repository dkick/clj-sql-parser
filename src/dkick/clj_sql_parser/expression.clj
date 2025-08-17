(ns dkick.clj-sql-parser.expression
  (:require
   [dkick.clj-sql-parser.multifn :as multifn])
  (:import
   (net.sf.jsqlparser.expression Function LongValue)
   (net.sf.jsqlparser.expression.operators.relational NotEqualsTo)
   (net.sf.jsqlparser.schema Column)
   (net.sf.jsqlparser.statement.select AllColumns)))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

(defmethod visit-before Object [_ context] context)

(defmethod visit-after AllColumns [sql-parsed context _]
  (assert (not (-> sql-parsed .getExceptColumns seq)))
  (assert (not (-> sql-parsed .getReplaceExpressions seq)))
  (swap! context conj :*))

(defmethod visit-after Column [sql-parsed context _]
  (swap! context conj (-> sql-parsed .getFullyQualifiedName keyword)))

(defmethod visit-before Function [_ _] (atom []))

(defmethod visit-after Function [sql-parsed context subcontext]
  (swap! context conj
         (with-meta
           (apply conj [(-> sql-parsed .getName keyword)] @subcontext)
           ;; Not totally sure this is necessary but it makes it
           ;; easier for us to understand in the SelectItem visitor if
           ;; we have a function or not
           {:type :sql-fn})))

(defmethod visit-after LongValue [sql-parsed context _]
  (swap! context conj (.getValue sql-parsed)))

;; TODO: Will this work with a BinaryExpression?
(defmethod visit-after NotEqualsTo [sql-parsed context _]
  (swap! context
         (fn [context']
           (let [operator (-> sql-parsed .getStringExpression keyword)
                 right    (peek context')
                 context' (pop context')
                 left     (peek context')
                 context' (pop context')]
             (conj context' [operator left right])))))
