(ns dkick.clj-sql-parser
  (:refer-clojure :exclude [format])
  (:require
   [honey.sql :as sql])
  (:import
   (dkick.clj_sql_parser ExpressionVisitorAdapter)
   (dkick.clj_sql_parser StatementVisitorAdapter)
   (dkick.clj_sql_parser.statement SelectVisitorAdapter)
   (dkick.clj_sql_parser.statement.select
    FromItemVisitorAdapter PivotVisitorAdapter SelectItemVisitorAdapter)
   (java.util.function Consumer)
   (net.sf.jsqlparser.parser CCJSqlParser CCJSqlParserUtil)
   (net.sf.jsqlparser.statement Statement)
   (org.apache.commons.lang3.builder
    MultilineRecursiveToStringStyle ReflectionToStringBuilder
    ToStringBuilder ToStringStyle)))

(defmulti format type)
(defmulti sql-honey type)
(defmulti sql-json type)

(defn parse
  ([s]
   (let [c (reify Consumer
             (accept [_ parser]
               (.withAllowComplexParsing ^CCJSqlParser parser)))]
     (parse s ^Consumer c)))
  ([s ^Consumer c] (CCJSqlParserUtil/parse s c)))

(defn make-visitors []
  (let [eva  (ExpressionVisitorAdapter.)
        pva  (PivotVisitorAdapter. eva)
        siva (SelectItemVisitorAdapter. eva)
        fiva (FromItemVisitorAdapter. eva)
        sva  (SelectVisitorAdapter. eva pva siva fiva)]
    {:expression-visitor  (doto eva (.setSelectVisitor sva))
     :from-item-visitor   (doto fiva (.setSelectVisitor sva))
     :pivot-visitor       pva
     :select-item-visitor siva
     :select-visitor      sva}))

(def visitors (make-visitors))

(defn make-statement-visitor
  ([] (make-statement-visitor (:select-visitor visitors)))
  ([select-visitor] (StatementVisitorAdapter. select-visitor)))

(def statement-visitor (make-statement-visitor))

(defmethod sql-honey String [s]
  (with-meta
    ;; Unfortunately the Java interop makes it difficult to avoid
    ;; using mutable data structures; i.e. contexts are passed into
    ;; functions but none of the default methods return them
    (let [context (atom [])]
      (-> s
          parse
          (.accept statement-visitor context)))
    {:type :sql/honey}))

(defmethod sql-honey Statement [x]
  (with-meta
    ;; Unfortunately the Java interop makes it difficult to avoid
    ;; using mutable data structures; i.e. contexts are passed into
    ;; functions but none of the default methods return them
    (let [context (atom [])]
      (.accept x statement-visitor context))
    {:type :sql/honey}))

(defmethod sql-json Statement [statement]
  ;; Java is making our lifes difficult here. MRTSS needs to sublcass
  ;; to access protected members: setUseClassName,
  ;; setUseIdentityHashCode. JSON_STYLE, which we want, is not
  ;; recursive. Mixing MRTSS and JSON_STYLE is not an option.
  (let [#_style
        #_ (doto (MultilineRecursiveToStringStyle.)
             #_(.setUseClassName true)
             #_(.setUseIdentityHashCode false)
             (ToStringBuilder/setDefaultStyle))

        style   ToStringStyle/JSON_STYLE
        builder (ReflectionToStringBuilder. statement style)]
    (.build builder)))

(defmethod sql-json String [s]
  (sql-json (parse s)))

(defmethod format :sql/honey [m]
  ;; Mostly just a pass through with defaulted options
  (sql/format m {:inline true}))

(defn println-sql-honey [{:keys [s]}]
  (println (sql-honey s)))

(comment
  [#'println-sql-honey]
  #__)
