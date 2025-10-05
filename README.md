# com.github.dkick/clj-sql-parser

Somewhat inspired by [Macaw](https://github.com/metabase/macaw), this
uses Java SQL Parser to transpile SQL into Honey SQL. This is
currently being tested against a number of DBT compiled SQL files
targetting Databricks SQL, and the testing has been targetted mostly
against those files, at the moment. Those files were proprietary, and
so I have not included them as part of any automated test suite; but I
have included an example of an SQL query with similar structure in the
automated test suite(s) when it produced an error. I also have not
been approaching this from a case-by-case analysis of the underlying
Java classes or by attempting to methodically process SQL grammer, but
rather running through example SQL, one by one, and so there will
definitely be gaps in the handling (so far). I no longer have access
to the original queries that kicked this off, so I'm not sure of what
change in direction to make to fill in the gaps, or even if I'll
make/find the motivation to keep up with this side quest. But ... it's
been an interesting exercise, nonetheless.

## Usage

    user> (require '[dkick.clj-sql-parser :as csp])
    nil
    user> (csp/sql-honey "SELECT * FROM t")
    {:select [:*], :from [:t]}
    user> 

Invoke a library API function from the command-line:

    $ clojure -X dkick.clj-sql-parser/println-sql-honey :s '"SELECT COUNT(*) <> 0 AS x FROM (SELECT a AS b FROM t AS u) WHERE x = 1"'
    {:select [[[:<> [:COUNT :*] 0] :x]], :from [[{:select [[:a :b]], :from [[:t :u]]}]], :where [:= :x 1]}

Run the project's tests:

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
