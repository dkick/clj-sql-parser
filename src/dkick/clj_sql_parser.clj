(ns dkick.clj-sql-parser
  (:require
   [dkick.clj-sql-parser.statement :refer [statement-visitor]])
  (:import
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

(defmulti sql->json type)

(defmethod sql->json Statement [statement]
  ;; Java is making our lifes difficult here. MRTSS needs to sublcass
  ;; to access protected members: setUseClassName,
  ;; setUseIdentityHashCode. JSON_STYLE, which we want, is not
  ;; recursive. Mixing MRTSS and JSON_STYLE is not an option.
  (let [#_#_style   (doto (MultilineRecursiveToStringStyle.)
                  #_(.setUseClassName true)
                  #_(.setUseIdentityHashCode false)
                  (ToStringBuilder/setDefaultStyle))
        style (ToStringStyle/JSON_STYLE)
        builder (ReflectionToStringBuilder. statement style)]
    (.build builder)))

(comment
  (.setUseClassName (MultilineRecursiveToStringStyle.) true)
  #_|)

(defmethod sql->json String [s]
  (sql->json (parse s)))
