(ns dkick.clj-sql-parser.statement.select.pivot
  (:require
   [dkick.clj-sql-parser.visitors :as visitors]))

(defmulti visit-after visitors/visit-after-group)
(defmulti visit-before visitors/visit-before-group)

(defmethod visit-before Object [_ sql-parsed context]
  [sql-parsed context])

