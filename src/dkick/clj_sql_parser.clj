(ns dkick.clj-sql-parser
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

(defn parse
  ([s]
   (let [c (reify Consumer
             (accept [_ parser]
               (.withAllowComplexParsing ^CCJSqlParser parser)))]
     (parse s ^Consumer c)))
  ([s ^Consumer c] (CCJSqlParserUtil/parse s c)))

(defn visitors []
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

(defn statement-visitor
  ([] (statement-visitor (:select-visitor (visitors))))
  ([select-visitor] (StatementVisitorAdapter. select-visitor)))

(def x-statement-visitor (statement-visitor))

(defmulti sql-honey type)

(defmethod sql-honey String [s]
  ;; Unfortunately the Java interop makes it difficult to avoid using
  ;; an atom
  (let [context (atom [])]
    (-> s
        parse
        (.accept x-statement-visitor context))))

(defmethod sql-honey Statement [x]
  ;; Unfortunately the Java interop makes it difficult to avoid using
  ;; an atom
  (let [context (atom [])]
    (.accept x x-statement-visitor context)))

(defn print-sql-honey [{:keys [s]}]
  (println (sql-honey s)))

(defmulti sql-json type)

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
