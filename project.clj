(defproject com.cognitect/contextual "0.1.0"
  :description "Associative values in Clojure and ClojureScript which
  know their context"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-beta1"]
                 [org.clojure/clojurescript "0.0-3196"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.6.3-SNAPSHOT"]]
                   :plugins [[lein-cljsbuild "1.0.5"]
                             [com.cemerick/clojurescript.test "0.3.3"]]
                   :aliases {"test-all" ["do" "test," "cljsbuild" "test"]}
                   :cljsbuild {:test-commands {"all" ["phantomjs" "target/test-runner.js"]}
                               :builds [{:source-paths ["src" "test"]
                                         :compiler {:output-to "target/test-runner.js"
                                                    :optimizations :whitespace
                                                    :pretty-print true}}]}}}
  )

