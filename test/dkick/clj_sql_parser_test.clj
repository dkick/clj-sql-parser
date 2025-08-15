(ns dkick.clj-sql-parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   ;; (S)ystem (U)nder (T)est
   [dkick.clj-sql-parser :as sut]
   [dkick.clj-sql-parser.statement :refer [statement-visitor]]
   [honey.sql :as sql]))

(defn reparse [sql-str]
  (let [sql-honey  (-> sql-str
                       sut/parse
                       (.accept statement-visitor (atom [])))
        [sql-str'] (sql/format sql-honey {:inline true})
        sql-honey' (-> sql-str'
                       sut/parse
                       (.accept statement-visitor (atom [])))]
    (assert (= sql-str sql-str'))
    (assert (= sql-honey sql-honey'))
    {:sql-str  sql-str,  :sql-honey  sql-honey
     :sql-str' sql-str', :sql-honey' sql-honey'}))

(defn reparse' [sql-str]
  (:sql-honey (reparse sql-str)))

(deftest select-test
  (testing "Select a literal 1"
    (is (= {:select [1]} (reparse' "SELECT 1")))))

