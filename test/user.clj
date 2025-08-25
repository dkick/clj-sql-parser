(ns user
  (:require
   [clojure.java.io :as io]
   [dkick.clj-sql-parser.statement.select.from-item.joins
    :refer [join-data]]
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

(comment
  [#'stop!]
  #__)
