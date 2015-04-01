
(ns miner.repl
  (:require clojure.repl))

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


;; inspired by runa-dev/homeless, but worth unrolling a bit
;; I like the explicit (seq m).
;; But really you should only use kwargs for REPL level command functions, not normal API
(defn apply-kw
  "Like apply but the last arg is a map containg keyword args that are unrolled before
  applying f"
  ([f] (f))
  ([f m] (apply f (apply concat (seq m))))
  ([f a m] (apply f a (apply concat (seq m))))
  ([f a b m] (apply f a b (apply concat (seq m))))
  ([f a b c & args+m] (apply f a b c (concat (butlast args+m) 
                                             (apply concat (seq (last args+m)))))))


;; hacked version of clojure.repl/find-doc which does too much
(defn rdoc
  "Prints documentation for any var whose name matches `regex`."
  [regex]
    (let [re (re-pattern regex)
          ms (concat (mapcat #(sort-by :name (map meta (vals (ns-interns %))))
                             (all-ns))
                     (map @#'clojure.repl/namespace-doc (all-ns))
                     (map @#'clojure.repl/special-doc (keys @#'clojure.repl/special-doc-map)))]
      (doseq [m (filter (fn [m] (and (:doc m) (re-matches re (str (:name m))))) ms)]
        (#'clojure.repl/print-doc m))))
