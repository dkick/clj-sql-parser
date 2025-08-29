(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'com.github.dkick/clj-sql-parser)
(def version "0.1.5")
#_ ; alternatively, use MAJOR.MINOR.COMMITS:
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")

(defn- pom-template [version]
  [[:description "FIXME: my new library."]
   [:url "https://github.com/dkick/clj-sql-parser"]
   [:licenses
    [:license
     [:name "Eclipse Public License"]
     [:url "http://www.eclipse.org/legal/epl-v10.html"]]]
   [:developers
    [:developer
     [:name "Dkick"]]]
   [:scm
    [:url "https://github.com/dkick/clj-sql-parser"]
    [:connection "scm:git:https://github.com/dkick/clj-sql-parser.git"]
    [:developerConnection
     "scm:git:ssh:git@github.com:dkick/clj-sql-parser.git"]
    [:tag (str "v" version)]]])

(defn- jar-opts [opts]
  (assoc
   opts
   :lib lib   :version version
   :jar-file  (format "target/%s-%s.jar" lib version)
   :basis     (b/create-basis {})
   :class-dir class-dir
   :target    "target"
   :src-dirs  ["src"]
   :pom-data  (pom-template version)

   :ns-compile
   '[dkick.clj-sql-parser.ExpressionVisitorAdapter
     dkick.clj-sql-parser.StatementVisitorAdapter
     dkick.clj-sql-parser.statement.SelectVisitorAdapter
     dkick.clj-sql-parser.statement.select.FromItemVisitorAdapter
     dkick.clj-sql-parser.statement.select.PivotVisitorAdapter
     dkick.clj-sql-parser.statement.select.SelectItemVisitorAdapter]))

(defn aot
  "AOT compile for gen-class"
  [opts]
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nCompiling" (:ns-compile opts) "...")
    (b/compile-clj opts)))

(defn test
  "Run all the tests."
  [opts]
  (aot opts)
  (let [basis    (b/create-basis {:aliases [:test]})
        cmds     (b/java-command
                  {:basis      basis
                    :main      'clojure.main
                    :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn ci
  "Run the CI pipeline of tests (and build the JAR)."
  [opts]
  (test opts)
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml...")
    (b/write-pom opts)
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println "\nBuilding JAR..." (:jar-file opts))
    (b/jar opts))
  opts)

(defn install
  "Install the JAR locally."
  [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)

(defn deploy
  "Deploy the JAR to Clojars."
  [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file (b/pom-path
                           (select-keys opts [:lib :class-dir]))}))
  opts)
