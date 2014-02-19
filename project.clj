(defproject com.velisco/halfbaked "0.2.2"
  :description "My collection of half-baked, semi-useful Clojure code."
  :url "http://github.com/miner/halfbaked"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:clj151 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :beta1 {:dependencies [[org.clojure/clojure "1.6.0-beta1"]]} }
  :repl-options {:init-ns miner.halfbaked}
  )

