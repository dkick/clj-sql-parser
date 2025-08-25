(ns dkick.clj-sql-parser-test.olio
  (:require
   [babashka.fs :as fs]
   [clojure.string :as str]
   [dkick.clj-sql-parser :as csp]))

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
  [#'file->sql-honey #'sql-file-seq #'sql-honey-ex-seq #'sql-parsed-ex-seq
   #'try-sql-honey #'try-sql-parsed]
  #__)
