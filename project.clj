(defproject visu "0.1.0"
  :description "some visualizations with cljs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/cljs" "src/clj"]

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [ring "1.2.0"]
                 [enlive "1.1.5"]
                 [http-kit "2.1.14"]
                 [cheshire "5.3.1"]
                 [compojure "1.1.6"]
                 [quil "1.6.0"]
                 [prismatic/dommy "0.1.2"]
                 [cljs-webgl "0.1.4-SNAPSHOT"]
                 [hiccups "0.3.0"]]

  :plugins [[lein-cljsbuild "1.0.1"]
            [com.cemerick/austin "0.1.3"]]

  :repl-options {:init-ns visu.server}

  :main visu.server
  :cljsbuild
  {:builds
   [{:source-paths ["src/cljs"]
     :compiler
     {:output-to "resources/public/js/main.js"
      :optimizations :simple
      :pretty-print true}}]})
