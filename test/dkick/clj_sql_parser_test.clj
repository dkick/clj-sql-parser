(ns dkick.clj-sql-parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   ;; (S)ystem (U)nder (T)est
   [dkick.clj-sql-parser :as sut]
   [honey.sql :as sql]))

(defn reparse [sql-str]
  (let [x-sql-honey  (sut/sql-honey sql-str)
        [sql-str']   (sql/format x-sql-honey {:inline true})
        x-sql-honey' (sut/sql-honey sql-str')]
    (assert (= sql-str sql-str'))
    (assert (= x-sql-honey x-sql-honey'))
    {:sql-str  sql-str,  :sql-honey  x-sql-honey
     :sql-str' sql-str', :sql-honey' x-sql-honey'}))

(defn get-sql-honey [sql-str]
  (:sql-honey (reparse sql-str)))

(deftest select-test
  (testing "Select a literal 1"
    (is (= {:select [1]} (get-sql-honey "SELECT 1"))))
  (testing "Select * from a table"
    (is (= {:select [:*], :from [:t]}
           (get-sql-honey "SELECT * FROM t")))))

(comment
  (sut/sql-honey "SELECT * FROM t")
  ;; => {:select [:*], :from [:t]}
  #_|)
