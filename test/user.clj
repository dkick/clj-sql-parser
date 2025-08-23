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

(defn sql-file-seq [dir]
  (->> (file-seq dir)
       (filter fs/regular-file?)
       (filter #(str/ends-with? % "sql"))))

(defn file->sql-honey [file]
  (-> (slurp file)
      (str/replace #"(?m)^\s*\n(?:\s*\n)*" "\n")
      csp/sql-honey))

(defn file->sql-parsed [file]
  (-> (slurp file)
      (str/replace #"(?m)^\s*\n(?:\s*\n)*" "\n")
      csp/parse))

(defn try-sql-honey [dir]
  (doseq [file (sql-file-seq dir)]
    (try (file->sql-honey file)
         (catch Throwable e
           (throw (ex-info "No honey!" {:file file} e))))))

(defn try-sql-parsed [dir]
  (doseq [file (sql-file-seq dir)]
    (try (file->sql-parsed file)
         (catch Throwable e
           (throw (ex-info "Not even parsed!" {:file file} e))))))

(defn sql-honey-ex-seq [dir]
  (->> (sql-file-seq dir)
       (map #(try (file->sql-honey %)
                  (catch Throwable e
                    (ex-info (ex-message e) {:file %} e))))))

(defn sql-parsed-ex-seq [dir]
  (->> (sql-file-seq dir)
       (map #(try (file->sql-parsed %)
                  (catch Throwable e
                    (ex-info (ex-message e) {:file %} e))))))

(comment
  #__)

(comment
  [::sql/_ ::sqh/_
   #'file->sql-honey #'join-data #'stop! #'sql-file-seq
   #'sql-honey-ex-seq #'sql-parsed-ex-seq #'try-sql-honey
   #'try-sql-parsed]
  #__)
