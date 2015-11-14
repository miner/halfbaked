(defproject com.velisco/halfbaked "0.3.0-SNAPSHOT"
  :description "My collection of half-baked, semi-useful Clojure code."
  :url "http://github.com/miner/halfbaked"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [criterium "0.4.3"]]
  :profiles {:clj16 {:dependencies [[org.clojure/clojure "1.6.0"]]} }
  :repl-options {:init-ns miner.halfbaked}
  )

