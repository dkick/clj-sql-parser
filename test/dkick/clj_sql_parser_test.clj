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
    (is (= sql-str sql-str'))
    (is (= x-sql-honey x-sql-honey'))
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
  (is (= {:select [:*]
          :from   [:t]}
         (get-sql-honey
          "SELECT * FROM t")))
  (is (= {:select-distinct [:a :b]
          :from            [:t]}
         (get-sql-honey
          "SELECT DISTINCT a, b FROM t")))
  (is (= {:select [:a]
          :from   [:t]}
         (get-sql-honey
          "SELECT a FROM t")))
  (is (= {:select [[:a :b]]
          :from   [[:t :u]]}
         (get-sql-honey
          "SELECT a AS b FROM t AS u")))
  (is (= {:select [:a :b]
          :from   [:t :u]}
         (get-sql-honey
          "SELECT a, b FROM t, u")))
  (is (= {:select  [:a :b]
          :from    [:t :u]
          :join-by [:left [:v [:= :t.c :v.c]]]}
         (get-sql-honey
          "SELECT a, b FROM t, u LEFT JOIN v ON t.c = v.c")))
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
  (is (= {:select [[[:<> [:COUNT :*] 0] :x]]
          :from   [:t]}
         (get-sql-honey
          ;; Annoyingly, Honey SQL format is turning a "!=" into
          ;; a "<>" here so the testing method in get-sql-honey from
          ;; reparse will only work with a "<>"
          "SELECT COUNT(*) <> 0 AS x FROM t")))
  (is (= {:select [:*]
          :from   [[{:select [[[:COUNT :*]]]
                     :from   [:t]}]]}
         (get-sql-honey
          "SELECT * FROM (SELECT COUNT(*) FROM t)")))
  (is (= {:select [:*]
          :from   [:t]
          :where  [:= :a 1]}
         (get-sql-honey
          "SELECT * FROM t WHERE a = 1")))
  (is (= {:select   [:a],
          :from     [[{:select [:*]
                       :from   [:t]
                       :where  [:= :b 1]}
                      :subquery]],
          :group-by [:a],
          :having   [:> [:COUNT :*] 1]}
         (get-sql-honey
          ;; The "AS subquery" is optional but sql/format produces a
          ;; string with it, so our test string uses it, too, so we
          ;; can compare versions in reparse
          (str "SELECT a FROM "
               "(SELECT * FROM t WHERE b = 1) AS subquery "
               "GROUP BY a HAVING COUNT(*) > 1"))))
  (is (= {:select
          [[[:COUNT :*] :a]
           [[:<> [:COUNT :*] 0] :b]
           [[:<> [:COUNT :*] 0] :c]]

          :from
          [[{:with
             [[:cte
               {:select   [:d]
                :from     [[{:select [:*]
                             :from   [:t]
                             :where  [:= :e 1]}
                            :u]]
                :group-by [:d]
                :having   [:> [:COUNT :*] 1]}]]

             :select [:*]
             :from   [:cte]}
            :v]]}
         (get-sql-honey
          (str
           "SELECT "
           "COUNT(*) AS a, COUNT(*) <> 0 AS b, COUNT(*) <> 0 AS c "
           "FROM (WITH cte AS "
           "(SELECT d FROM (SELECT * FROM t WHERE e = 1) AS u "
           "GROUP BY d HAVING COUNT(*) > 1) "
           "SELECT * FROM cte) AS v")))))

(comment
  (-> (sut/sql-honey "SELECT (COUNT(*)) AS A")
  ;; => Execution error (IllegalArgumentException) at dkick.clj-sql-parser.ExpressionVisitorAdapter/-visit (ExpressionVisitorAdapter.clj:12).
  ;;    No method in multimethod 'visit-after' for dispatch value: class net.sf.jsqlparser.expression.operators.relational.ParenthesedExpressionList
      #_|)

  (sut/sql-honey
   "
-- This test fails if any relevant cost center has '<NA>' for its division.

-- This test fails if any relevant cost center has '<NA>' for its company.
SELECT DISTINCT
    dc.costcenter_key,
    dc.cc_company
FROM `dev_silver`.`dev_dkick_dimensions`.`dim_costcenter` AS dc
LEFT JOIN `dev_silver`.`dev_dkick_facts`.`fact_orders` AS fo
  ON fo.order_costcenter_key = dc.costcenter_key AND fo.st_row_current = 1
WHERE dc.st_row_current = 1
  AND dc.costcenter_identifier <> '<NA>'
  -- ignore costcenters from orders before 1/1/23
  AND (fo.order_costcenter_key IS NULL OR fo.order_entry_date >= '2023-01-01')
  -- a row returned here will fail the test
  AND dc.cc_company = '<NA>'")

  (-> (sut/sql-honey "SELECT DISTINCT a, b FROM t")
      (sql/format {:inline true}))

  (-> (sut/sql-honey "SELECT a, b FROM t, u LEFT JOIN v ON t.c = v.c")
      #_(sql/format {:ineline true}))
  ;; => {:select [:a :b], :from [:t :u], :join-by [:left [:v [:= :t.c :v.c]]]}
  ;; => ["SELECT a, b FROM t, u LEFT JOIN v ON t.c = v.c"]
  #_|)
