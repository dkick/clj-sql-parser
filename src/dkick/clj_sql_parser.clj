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
  (let [ev  (ExpressionVisitorAdapter.)
        pv  (PivotVisitorAdapter. ev)
        siv (SelectItemVisitorAdapter. ev)
        fiv (FromItemVisitorAdapter. ev)
        sv  (SelectVisitorAdapter. ev pv siv fiv)]
    {:expression-visitor  (doto ev (.setSelectVisitor sv))
     :from-item-visitor   (doto fiv (.setSelectVisitor sv))
     :pivot-visitor       pv
     :select-item-visitor siv
     :select-visitor      sv}))

(def visitors (make-visitors))

(defn make-statement-visitor
  ([] (make-statement-visitor (:select-visitor visitors)))
  ([select-visitor] (StatementVisitorAdapter. select-visitor)))

(def statement-visitor (make-statement-visitor))

(defmethod sql-honey String [s]
  (with-meta
    ;; Unfortunately the Java interop makes it difficult to avoid
    ;; using an atom
    (let [context (atom [])]
      (-> s
          parse
          (.accept statement-visitor context)))
    {:type :sql/honey}))

(defmethod sql-honey Statement [x]
  (with-meta
    ;; Unfortunately the Java interop makes it difficult to avoid
    ;; using an atom
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
  (when-let [kv (seq (select-keys m [:create-view
                                     :create-materilized-view
                                     :create-or-replace-view]))]
    (throw (ex-info "N/A" {:keys (keys kv)
                           :m    m})))
  (sql/format m {:inline true}))

(defn println-sql-honey [{:keys [s]}]
  (println (sql-honey s)))

(comment
  [#'println-sql-honey]
  #__)
