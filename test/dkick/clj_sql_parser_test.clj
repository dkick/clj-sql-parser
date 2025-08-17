(ns dkick.clj-sql-parser-test
  (:require
   [clojure.test :refer [deftest is]]
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
  (is (= {:select [1]}
         (get-sql-honey
          "SELECT 1")))
  (is (= {:select [:*], :from [:t]}
         (get-sql-honey
          "SELECT * FROM t")))
  (is (= {:select [:a], :from [:t]}
         (get-sql-honey
          "SELECT a FROM t")))
  (is (= {:select [[:a :b]], :from [:t]}
         (get-sql-honey
          "SELECT a AS b FROM t")))
  (is (= {:select [:a :b], :from [:t :u]}
         (get-sql-honey
          "SELECT a, b FROM t, u")))
  (is (= {:select [:*]
          :from   [[{:select [:*]
                     :from   [:t]}]]}
         (get-sql-honey
          "SELECT * FROM (SELECT * FROM t)")))
  (is (= {:select [[[:COUNT :*]]]
          :from   [:t]}
         (get-sql-honey
          "SELECT COUNT(*) FROM t")))
  (is (= {:select [[[:COUNT :*] :n]]
          :from   [:t]}
         (get-sql-honey
          "SELECT COUNT(*) AS n FROM t")))
  (is (= {:select [:*]
          :from   [[{:select [[[:COUNT :*]]]
                     :from   [:t]}]]}
         (get-sql-honey
          "SELECT * FROM (SELECT COUNT(*) FROM t)"))))

(comment
  (def s
    "
    select
      count(*) as failures,
      count(*) != 0 as should_warn,
      count(*) != 0 as should_error
    from (
with validation_errors as (
    select
        cust_location_id
    from (select * from `dev_bronze`.`dev_dkick_ftp2`.`vw_arss_customerdirectory_export` where st_row_current = 1) dbt_subquery
    group by cust_location_id
    having count(*) > 1
)
select *
from validation_errors
    ) dbt_internal_test
")

  (sut/sql-honey s)
  #_|)
