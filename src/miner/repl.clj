
(ns miner.repl)

;; https://gist.github.com/2049970
;; ghoseb via cgrand

;; bug -- doesn't notice name collisions for methods in multiple interfaces
(defn scaffold
  "Print the ancestor method signatures of a given interface."
  [iface]
  (doseq [[iface methods] (->> ^Class iface
                               .getMethods
                               (map (fn [^java.lang.reflect.Method meth] 
                                      (vector (.getName (.getDeclaringClass meth))
                                              (symbol (.getName meth))
                                              (count (.getParameterTypes meth)))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into ['this] (map #(symbol (str "arg" %)) (range 1 (inc argcount))))))))))
