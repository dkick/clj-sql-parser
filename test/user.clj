(ns user
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [dkick.clj-sql-parser :as csp]
   [dkick.clj-sql-parser.statement.select.from-item.joins
    :refer [join-data]]
   [honey.sql :as sql]
   [honey.sql.helpers :as sqh]
   [malli.dev :as m.dev]
   [malli.dev.pretty :as m.dev.pretty]))

;; This is a snippet that loads all user.clj's it can find.  If *file*
;; is nil, it means we are called recursively, do nothing.
(when *file*
  (->> (.getResources (.getContextClassLoader (Thread/currentThread))
                      "user.clj")
       enumeration-seq
       ;; Assume the first user.clj is the currently loaded one. Load
       ;; others if there are more to load.
       rest
       (run! #(clojure.lang.Compiler/load (io/reader %)))))

;;; Below is the regular content of this project-local user.clj

(defn start!
  []
  (m.dev/start! {:report (m.dev.pretty/reporter)}))

(defn stop!
  []
  (m.dev/stop!))

(start!)

(defn sql-file-seq [x]
  (->> (file-seq x)
       (filter fs/regular-file?)
       (filter #(str/ends-with? % "sql"))))

(defn file->sql-honey [x]
  (-> (slurp x)
      (str/replace #"(?m)^\s*\n(?:\s*\n)*" "\n")
      csp/sql-honey))

(defn try-sql-honey [x]
  (doseq [x (sql-file-seq x)]
    (try (file->sql-honey x)
         (catch Exception e
           (throw (ex-info "No honey!" {:file x} e))))))

(comment
  #__)

(comment
  [::sql/_ ::sqh/_
   #'file->sql-honey #'join-data #'stop! #'sql-file-seq]
  #__)
