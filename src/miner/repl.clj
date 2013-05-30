
(ns miner.repl)

;; https://gist.github.com/2049970
;; ghoseb via cgrand

;; bug -- doesn't notice name collisions for methods in multiple interfaces
(defn scaffold
  "Print the ancestor method signatures of a given interface."
  [iface]
  (doseq [[iface methods] (->> iface
                               .getMethods
                               (map #(vector (.getName (.getDeclaringClass %))
                                             (symbol (.getName %))
                                             (count (.getParameterTypes %))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into ['this] (map #(symbol (str "arg" %)) (range 1 (inc argcount))))))))))
