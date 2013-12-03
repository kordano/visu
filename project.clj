(defproject visu "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/cljs" "src/clj"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1934"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [ring "1.2.0"]
                 [enlive "1.1.1"]
                 [http-kit "2.1.12"]
                 [cheshire "5.2.0"]
                 [compojure "1.1.5"]
                 [quil "1.6.0"]
                 [prismatic/dommy "0.1.1"]
                 [hiccups "0.2.0"]]

  :plugins [[lein-cljsbuild "0.3.2"]
            [com.cemerick/austin "0.1.1"]]

  :repl-options {:init-ns visu.server}

  :main visu.server
  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs"]
     :compiler
     {:output-to "resources/public/js/main.js"
      :optimizations :simple
      :pretty-print true}}]})
