(ns dkick.clj-sql-parser.schema
  (:require
   [clojure.string :as str])
  (:import
   (net.sf.jsqlparser.schema MultiPartName)))

(defmulti get-fully-qualified-name type)

(defmethod get-fully-qualified-name MultiPartName [sql-parsed]
  (let [fqn (.getFullyQualifiedName sql-parsed)
        fqn (->> (str/split fqn #"\.")
                 (map MultiPartName/unquote)
                 (str/join "."))]
    (keyword fqn)))
