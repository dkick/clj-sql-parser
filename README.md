# com.github.dkick/clj-sql-parser

Inspired by [Macaw](https://github.com/metabase/macaw), this uses Java
SQL Parser to transpile SQL into Honey SQL. This is currently being
tested against a number of DBT compiled SQL files targetting
Databricks SQL, and the testing has been targetted mostly against
those files, at the moment.

## Usage

    user> (require '[dkick.clj-sql-parser :as csp])
    nil
    user> (csp/sql-honey "SELECT * FROM t")
    {:select [:*], :from [:t]}
    user> 

Invoke a library API function from the command-line:

    $ clojure -X dkick.clj-sql-parser/print-sql-honey :s '"SELECT COUNT(*) <> 0 AS x FROM (SELECT a AS b FROM t AS u) WHERE x = 1"'
    {:select [[[:<> [:COUNT :*] 0] :x]], :from [[{:select [[:a :b]], :from [[:t :u]]}]], :where [:= :x 1]}

Run the project's tests (they'll fail until you edit them):

    $ clojure -T:build test

Run the project's CI pipeline and build a JAR (this will fail until you edit the tests to pass):

    $ clojure -T:build ci

This will produce an updated `pom.xml` file with synchronized dependencies inside the `META-INF`
directory inside `target/classes` and the JAR in `target`. You can update the version (and SCM tag)
information in generated `pom.xml` by updating `build.clj`.

Install it locally (requires the `ci` task be run first):

    $ clojure -T:build install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment
variables (requires the `ci` task be run first):

    $ clojure -T:build deploy

Your library will be deployed to com.github.dkick/clj-sql-parser on clojars.org by default.

## License

Copyright © 2025 Dkick

Distributed under the GNU GPL version 3.
