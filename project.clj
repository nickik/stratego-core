(defproject stratego-core "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.match "0.2.1"]
                 [liberator "0.10.0"]
                 [compojure "1.1.6"]
                 [hiccup "1.0.4"]
                 [print-foo "0.4.2"]
                 #_[reiddraper/simple-check "0.5.5"]]
  :plugins [[lein-ring "0.8.10"]]
  :ring {:handler stratego-core.server/handler
         :port 80
         :auto-reload? true
         :auto-refresh? true})
