(ns dkick.clj-sql-parser-test
  (:require
   [clojure.test :refer [deftest is testing]]
   ;; (S)ystem (U)nder (T)est
   [dkick.clj-sql-parser :as sut]
   [dkick.clj-sql-parser.statement :refer [statement-visitor]]
   [honey.sql :as sql]))

(def the-statement-visitor (statement-visitor))

(defn reparse [sql-str]
  (let [sql-honey  (-> sql-str
                       sut/parse
                       (.accept the-statement-visitor (atom [])))
        [sql-str'] (sql/format sql-honey {:inline true})
        sql-honey' (-> sql-str'
                       sut/parse
                       (.accept the-statement-visitor (atom [])))]
    (assert (= sql-str sql-str'))
    (assert (= sql-honey sql-honey'))
    {:sql-str  sql-str,  :sql-honey  sql-honey
     :sql-str' sql-str', :sql-honey' sql-honey'}))

(defn sql-honey [sql-str]
  (:sql-honey (reparse sql-str)))

(deftest select-test
  (testing "Select a literal 1"
    (is (= {:select [1]} (sql-honey "SELECT 1")))))

(comment
  (-> (sut/parse "SELECT 1")
      (.accept the-statement-visitor (atom [])))
  #_|)
