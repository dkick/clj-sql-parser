(ns dkick.clj-sql-parser-test
  (:require
   [clojure.test :refer [deftest is]]
   ;; (S)ystem (U)nder (T)est
   [dkick.clj-sql-parser :as sut]
   [honey.sql :as sql]
   [honey.sql.helpers :as sqh]))

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

(def dstnct :select-distinct)

(deftest select-test
  (is (= {:select [1]}
         (get-sql-honey
          "SELECT 1")))
  (is (= {:select [[[:+ 1 2 3 4]]]}
         (get-sql-honey
          "SELECT 1 + 2 + 3 + 4")))
  (is (= {:select [:*]
          :from   [:t]}
         (get-sql-honey
          "SELECT * FROM t")))
  (is (= {:select [[:* :except [:a]]]
          :from   [:t]}
         (get-sql-honey
          "SELECT * EXCEPT (a) FROM t")))
  (is (= {dstnct [:a :b]
          :from  [:t]}
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
  (is (= {:select [:a],
          :from   [:t],
          :where  [:not-in :a {:select [:a]
                               :from   [:u]}]}
         (get-sql-honey
          "SELECT a FROM t WHERE a NOT IN (SELECT a FROM u)")))
  (is (= {:select [:a]
          :from   [:t]
          :where  [:not-in :a ["a" "b" "c"]]}
         (get-sql-honey
          "SELECT a FROM t WHERE a NOT IN ('a', 'b', 'c')")))
  (is (= {:select   [:a]
          :from     [[{:select [:*]
                       :from   [:t]
                       :where  [:= :b 1]}
                      :subquery]]
          :group-by [:a]
          :having   [:> [:COUNT :*] 1]}
         (get-sql-honey
          ;; The "AS subquery" is optional but sql/format produces a
          ;; string with it, so our test string uses it, too, so we
          ;; can compare versions in reparse
          (str "SELECT a FROM "
               "(SELECT * FROM t WHERE b = 1) AS subquery "
               "GROUP BY a HAVING COUNT(*) > 1"))))
  (is (= {:select [[[:COUNT :*] :a]
                   [[:<> [:COUNT :*] 0] :b]
                   [[:<> [:COUNT :*] 0] :c]]
          :from   [[{:with
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
           "SELECT * FROM cte) AS v"))))
  (is (= {:from [[:t1 :u]]

          :join-by
          [:inner
           [[{:select [:*]
              :from   [:t2]
              :where  [:and [[:= :c "U"]] [[:<> :d nil]]]}
             :v]
            [:and
             [[:= :u.e [:CONCAT "A" :v.d "-" [:UPPER :v.f]]]]
             [[:= :v.g 1]]]]]

          :where [:and [[:= :u.g 1]] [[:= :u.b "<NA>"]]]
          dstnct [:u.a :u.b]}
         (get-sql-honey
          (str
           "SELECT DISTINCT u.a, u.b "
           "FROM t1 AS u "
           "INNER JOIN "
           "(SELECT * FROM t2 WHERE (c = 'U') AND (d IS NOT NULL)) AS v "
           "ON (u.e = CONCAT('A', v.d, '-', UPPER(v.f))) AND (v.g = 1) "
           "WHERE (u.g = 1) AND (u.b = '<NA>')"))))
  (is (= {:select [:*]
          :from   [:data_with_change_hash]

          :qualify
          [:or
           [[:<>
             :_row_hash
             [:over
              [[:LAG :_row_hash]
               {:partition-by [:cono :division_number],
                :order-by     [:_ingest_timestamp]}]]]]
           [[:=
             [:over
              [[:LAG :_row_hash]
               {:partition-by [:cono :division_number],
                :order-by     [:_ingest_timestamp]}]]
             nil]]]}
         (get-sql-honey
          (str
           "SELECT * FROM data_with_change_hash "
           "QUALIFY "
           "(_row_hash <> LAG(_row_hash) "
           "OVER "
           "(PARTITION BY cono, division_number "
           "ORDER BY _ingest_timestamp ASC)) "
           "OR "
           "(LAG(_row_hash) "
           "OVER "
           "(PARTITION BY cono, division_number "
           "ORDER BY _ingest_timestamp ASC) IS NULL)"))))
  (is (= {:select
          [[[:COALESCE
             [:over
              [[:LEAD [:COALESCE :a [:cast "1900-01-01" :timestamp]]]
               {:partition-by [:b :c]
                :order-by
                [[[:COALESCE :a [:cast "1900-01-01" :timestamp]]]]}]]
             [:cast "2200-01-01" :timestamp]]
            :z]]

          :from [:t]}
         (get-sql-honey
          (str
           "SELECT "
           "COALESCE("
           "LEAD(COALESCE(a, CAST('1900-01-01' AS TIMESTAMP))) OVER ("
           "PARTITION BY b, c "
           "ORDER BY COALESCE(a, CAST('1900-01-01' AS TIMESTAMP)) ASC"
           "), "
           "CAST('2200-01-01' AS TIMESTAMP)"
           ") AS z "
           "FROM t"))))
  (is (= {:union
          [{:intersect
            [{:union
              [{:select [:*]
                :from   [:t1]}
               {:select [:*]
                :from   [:t2]}
               {:select [:*]
                :from   [:t3]}
               {:select [:*]
                :from   [:t4]}]}
             {:select [:*]
              :from   [:t5]}]}
           {:select [:*]
            :from   [:t6]}]

          :order-by [:a :b]}
         (get-sql-honey
          (str
           "SELECT * FROM t1 UNION "
           "SELECT * FROM t2 UNION "
           "SELECT * FROM t3 UNION "
           "SELECT * FROM t4 INTERSECT "
           "SELECT * FROM t5 UNION "
           "SELECT * FROM t6 "
           "ORDER BY a ASC, b ASC")))))

(comment
  [::sqh/_]

  (-> "SELECT 1+2+3+4"
      sut/sql-honey
      (sql/format {:inline true}))

  (-> {:select [[[:+ 1 2 3 4]]]}
      (sql/format {:inline true}))
  #__)
