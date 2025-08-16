(ns dkick.clj-sql-parser.statement.select.pivot
  (:require
   [dkick.clj-sql-parser.multifn :as multifn]))

(defmulti visit-after multifn/visit-subcontext-group)
(defmulti visit-before multifn/visit-context-group)

