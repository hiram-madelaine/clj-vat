(defproject clj-vat "0.1.2"
  :description "A Clojure(Script) library designed to validate European VAT code"
  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store"]
  :url "https://github.com/hiram-madelaine/clj-vat"
  :source-paths ["src/cljx"]
  :test-paths ["target/test-classes"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [org.clojure/test.check "0.5.8"]
                 [com.cemerick/double-check "0.5.7"]]
  :jvm-opts ["-Xmx1g" "-server"]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}
  :hooks [cljx.hooks]
  :cljsbuild {:test-commands {"phantomjs" ["phantomjs" :runner "target/testable.js"]
                              "node" ["node" :node-runner "target/testable.js"]}
              :builds [{:source-paths ["target/classes" "target/test-classes"]
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced
                                   :pretty-print true
                                   :libs [""]}}]}

  :profiles {:dev {:plugins [[com.keminglabs/cljx "0.4.0"]
                             [lein-cljsbuild "1.0.4-SNAPSHOT"]
                             [lein-cooper "0.0.1"]
                             [com.cemerick/clojurescript.test "0.3.1"]]
                   :aliases {"cleantest" ["do" "clean," "cljx" "once," "test,"
                                          "cljsbuild" "test"]
                             "deploy" ["do" "clean," "cljx" "once," "deploy" "clojars"]}}})
