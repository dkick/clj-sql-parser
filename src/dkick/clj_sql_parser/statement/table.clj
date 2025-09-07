(ns dkick.clj-sql-parser.statement.table)

(defn create-table [that sql-parsed context]
  (throw (ex-info "N/A" #t {:that that
                            :sql-parsed sql-parsed
                            :context    context})))
