(defproject hundred-pushups "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [clj-time "0.13.0"]
                 [com.andrewmcveigh/cljs-time "0.4.0"]
                 [core-async-storage "0.2.0"]
                 [day8.re-frame/async-flow-fx "0.0.6"]
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]
                 [re-frame "0.9.2"]
                 [reagent "0.6.0" :exclusions [cljsjs/react cljsjs/react-dom cljsjs/react-dom-server]]
                 ;; react-native-datepicker uses
                 ;; moment, and we need to convert to/from it
                 [cljsjs/moment "2.17.1-0"]]
  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-figwheel "0.5.9"]]
  :clean-targets ["target/" "index.ios.js" "index.android.js"]
  :aliases {"prod-build" ^{:doc "Recompile code with prod profile."}
            ["do" "clean"
             ["with-profile" "prod" "cljsbuild" "once" ]]}
  :profiles {:project-tools {:plugins [[com.jakemccrary/lein-test-refresh "0.18.0"]
                                       [venantius/ultra "0.5.0"]]
                             :dependencies [[proto-repl-charts "0.3.2"]
                                            [proto-repl "0.3.1"]]}
             :dev {:dependencies [[figwheel-sidecar "0.5.9"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [org.clojure/test.check "0.9.0"]
                                  [com.gfredericks/test.chuck "0.2.7"]]
                   :plugins [[lein-doo "0.1.7"]]
                   :source-paths ["src" "env/dev"]
                   :cljsbuild    {:builds [{:id           "ios"
                                            :source-paths ["src" "env/dev"]
                                            :figwheel     true
                                            :compiler     {:output-to     "target/ios/not-used.js"
                                                           :main          "env.ios.main"
                                                           :output-dir    "target/ios"
                                                           :optimizations :none}}
                                           {:id           "android"
                                            :source-paths ["src" "env/dev"]
                                            :figwheel     true
                                            :compiler     {:output-to     "target/android/not-used.js"
                                                           :main          "env.android.main"
                                                           :output-dir    "target/android"
                                                           :optimizations :none}}
                                           {:id            "node-test"
                                            :source-paths  ["src" "test"]
                                            :compiler      {:output-to "target/testable.js"
                                                            :output-dir "target"
                                                            :main hundred-pushups.doo-runner
                                                            :target :nodejs}}]}
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}
             :prod {:cljsbuild {:builds [{:id           "ios"
                                          :source-paths ["src" "env/prod"]
                                          :compiler     {:output-to     "index.ios.js"
                                                         :main          "env.ios.main"
                                                         :output-dir    "target/ios"
                                                         :language-in :ecmascript5
                                                         :static-fns    true
                                                         :optimize-constants true
                                                         :optimizations :simple
                                                         :closure-defines {"goog.DEBUG" false}}}
                                         {:id            "android"
                                          :source-paths ["src" "env/prod"]
                                          :compiler     {:output-to     "index.android.js"
                                                         :main          "env.android.main"
                                                         :output-dir    "target/android"
                                                         :language-in :ecmascript5
                                                         :static-fns    true
                                                         :optimize-constants true
                                                         :optimizations :simple
                                                         :closure-defines {"goog.DEBUG" false}}}]}}})
