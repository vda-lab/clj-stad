(defproject gorilla-test "0.1.0-SNAPSHOT"
  :description "A test project for Gorilla REPL."
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.csv "0.1.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [proto-repl-charts "0.3.2"]
                 [ubergraph "0.5.2"]
                 [aysylu/loom "1.0.2"]
                 [kixi/stats "0.5.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [incanter/incanter "1.9.3"]
                 [incanter-gorilla "0.1.0"]
                 [progrock "0.1.2"]
                 [org.clojure/core.async "0.4.474"]]
  :main ^:skip-aot gorilla-test.core
  :target-path "target/%s"
;  :plugins [[lein-gorilla "0.4.0"]]
  :profiles {:uberjar {:aot :all}})
