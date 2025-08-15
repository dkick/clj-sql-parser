(ns dkick.clj-sql-parser.multifn)

(defn visit-group
  [sql-parsed _context]
  (type sql-parsed))
