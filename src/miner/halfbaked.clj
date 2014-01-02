(ns miner.halfbaked
  (:require [clojure.string :as str]))

;; See  clojure.lang.Compiler/CHAR_MAP for the official list of conversions
;; Some don't happen in normal code so I elided them.
(def demangle-replacements
  (array-map "_QMARK_" "?"
             "_BANG_" "!"
             "_STAR_" "*"
             "_GT_" ">"
             "_EQ_" "="
             "_PLUS_" "+"
             "_LT_" "<"
             "_SLASH_" "/"
             "_AMPERSAND_" "&"
             "_TILDE_" "~"
             ;; keep underbar last
             "_" "-"))

;; a faster but ugly version is in demangle.clj
(defn ^String demangle
  "Demangle a clojure identifier name"
  [^String s]
  (reduce-kv str/replace s demangle-replacements))

(defn compiled-fn-name
  "returns the simple name (a string) for the given function f as determined by the compiler"
  [f]
  (let [f (if (var? f) (var-get f) f)
        strname (when (fn? f) (str f))]
    (when strname
      (demangle ((re-find #"[$](.*)@" strname) 1)))))


;; original:
;; http://groups.google.com/group/clojure/browse_thread/thread/234ac3ff0a4b6b80?pli=1
;; but slightly changed for Clojure updates since 1.0

(defn demangle-class-name
  "Given the name of a class that implements a Clojure function, returns the function's
   name in Clojure. Note: If the true Clojure function name
   contains any underscores (a rare occurrence), the unmangled name will
   contain hyphens at those locations instead."
  [class-name]
  (demangle (clojure.string/replace class-name #"^(.+)\$([^@]+)(|@.+)$" "$1/$2")))

;; only appropriate for debugging
(defmacro current-fn-name []
  "Returns a string, the name of the current Clojure function"
  `(-> (Throwable.) .getStackTrace first .getClassName demangle-class-name))

(defmacro not-implemented []
  `(throw (Error. (str "fn " (current-fn-name) " not implemented"))))

;; Baishampayan Ghose  b.ghose@gmail.com
(defmacro with-timeout [ms & body]
 `(let [f# (future ~@body)]
   (.get #^java.util.concurrent.Future f# ~ms java.util.concurrent.TimeUnit/MILLISECONDS)))


;; Defines a Dynamic var (Clojure 1.3+)
;; Not the same as the old clojure.contrib.def/defvar version.
(defmacro defdynamic
  "Defines a dynamic var with an optional intializer and doc string.  The naming convention
is to use *earmuffs*, but that is not enforced."
  ([name]
     (list `def (with-meta name (assoc (meta name) :dynamic true))))
  ([name init]
     (list `def (with-meta name (assoc (meta name) :dynamic true)) init))
  ([name doc-string init]
     (list `def (with-meta name (assoc (meta name) :doc doc-string :dynamic true)) init)))


;; expects numbers as result of keyfn, but special cases nil arg and nil result, treating
;; them as MIN_VALUE for convenience
(defn index-of-max [keyfn coll]
  (let [fk #(or (and % (keyfn %)) Long/MIN_VALUE)]
    (when-let [s (seq coll)]
      (loop [mi 0 mv (fk (first s)) i 0 cs (next s)]
        (if-let [cs (seq cs)]
          (let [v (fk (first cs))]
            (let [i (inc i)]
              (if (> mv v)
                (recur mi mv i (next cs))
                (recur i v i (next cs)))))
          mi)))))


(defn index-when [f coll]
  "Returns index of first item in coll for which (f item) is truthy, or nil if none is found."
  (first (keep-indexed #(when (f %2) %1) coll)))

(defn index-of [val coll]
  "Returns index of item = val in coll, or nil if is equal."
  (index-when #(= val %) coll))


(defmacro when-let* [[b init & bs] & body]
  "Extended version of when-let that allows additional let bindings
  when the first has a truthy value."
  `(when-let [~b ~init]
     (let [~@bs]
       ~@body)))

;; George Jahad <clojure@blackbirdsystems.net>
;; Macro for enabling circular references if you really want to do
;; that sort of thing.
(defmacro remote-declare [n]
 (let [ns (namespace n)
       v (name n)
       orig-ns (str *ns*)]
   `(do (in-ns '~(symbol ns))
        (declare ~(symbol v))
        (in-ns '~(symbol orig-ns)))))

;; Lots of versions of this floating around the web
;; Some hack the keys as well.  Not mine.  cgrand suggested the
;; (transient map) as the initial value to speed things up a bit.
(defn mapmap [f mp]
  (persistent!
   (reduce-kv (fn [tm k v] (assoc! tm k (f v)))
              (transient mp)
              mp)))

;; Returns map with keys replaced by calling (kf k) on each key.  Values are unchanged.
(defn mapk [kf mp] 
  (persistent! (reduce-kv (fn [m k v] (assoc! m (kf k) v)) (transient {}) mp)))

(defn rotate
  "Returns a lazy seq of the elements of the finite sequence `coll` with
  the first `n` elements rotated to the end of the result.  If n is
  negative, the last n elements of coll are rotated to the front of
  the result.  Default n is 1 if not given."
  ([coll] (rotate 1 coll))
  ([n coll]
       (let [cnt (count coll)
             n (if (<= cnt 1) 0 (mod n cnt))]
         (concat (drop n coll) (take n coll)))))


;; probably not so efficient since it walks twice
(defn kvs-map [fkey fval coll]
  (zipmap (map fkey coll) (map fval coll)))

;; probably better
(defn map-by [fkey fval coll]
  (into {} (map (juxt fkey fval) coll)))

;; consider reduce-kv as an alternative if you're going to reduce the result


;; inspired by ninjudd/clojure-useful (which has moved to flatland/useful I think)
(defn update
  "Update value in map where f is a function that takes the old value and the supplied args and
   returns the new value. For efficiency, does not change map if the old value is the same as the
   new value. If key is sequential, update all keys in the sequence with the same function."  
  [map key f & args]
  (if (sequential? key)
    (reduce #(apply update %1 %2 f args) map key)
    (let [old (get map key)
          new (apply f old args)]
      (if (identical? old new) map (assoc map key new)))))


;; Like standard interleave but doesn't drop excess elements; also works with zero or one
;; arg.  Of course, this should not be used with infinite sequences.
(defn interleave-all
  "Returns a lazy seq of the first item in each collection, then the second, etc.  If one collection ends,
continues to interleave the others."
  ([] nil)
  ([c] (lazy-seq c))

  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (if (and s1 s2)
          (cons (first s1) (cons (first s2) 
                                 (interleave-all (rest s1) (rest s2))))
	  (or s1 s2)))))

  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (if (every? identity ss)
          (concat (map first ss) (apply interleave-all (map rest ss)))
	  (apply interleave-all (filter identity ss)))))))


;; reference: http://www.rgagnon.com/javadetails/java-0456.html
(defn asciify [s]
  "Returns an ASCII string approximation of the string `s`"
  (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
        diacriticals #"\p{InCombiningDiacriticalMarks}+"]
    (clojure.string/replace normalized diacriticals "")))

;; works better than asciify for docs converted from MS Word or FileMaker
(defn str-clean [st]
  (str/escape (str/trim st) {\u2019 \' \u2018 \' \u201c \" \u201d \" \u2013 \- \u2014 \-}))


(defn zmod? [num divisor]
  "Returns true iff `num` is a multiple of `divisor`"
  (zero? (mod num divisor)))

;; not the best thing to use on known vectors
(defn first= [xs y]
  (and (sequential? xs) (= (first xs) y)))

;; untested

;; capture the default so that binding doesn't break the debug fn
(def ^:dynamic *debug* *err*)

;; returns true to be convenient in pre- and post-conditions
(defn debug-args [& args]
  (binding [*out* *debug*]
    (apply println args))
  true)

;; %0 is bound to the function's name within the function.  Useful for pre-conditions,
;; logging and error reporting.
(defmacro defn0 [name & fdcls]
  `(let [~'%0 (symbol (name (ns-name *ns*)) (name '~name))]
     (defn ~name ~@fdcls)))


;;; From Meikel Brandmeyer (kotarak) <mb@kotka.de> on the clojure mailing list:
;;; You can call with-resource recursively. This has the added benefit of closing the resources in reverse order.
(defmacro with-resources
  [bindings close-fn & body]
  (let [[x v & more] bindings]
    `(let [~x ~v]
       (try
         ~(if-let [more (seq more)]
            `(with-resources ~more ~close-fn ~@body)
            `(do ~@body))
         (finally
           (~close-fn ~x))))))

;; Simple and works
(defmacro basis [record-type-symbol] `(. ~record-type-symbol getBasis))

;; based on an idea by alan@malloys.com (he called it `?`, but I changed to `debug`)
(defmacro debug [x]
 (let [line (:line (meta &form))
       file *file*]
   `(let [x# ~x]
      (binding [*print-length* 10
                *print-level* 3]
      (println "Debug:" (pr-str '~x) "is" (pr-str x#)
               (str "; (" ~file ":" ~line ")"))
      (flush)
      x#))))


(defn duplicates
  "Returns the set of duplicates in coll."
  [coll]
  (loop [seen #{} dups #{} coll coll]
    (if-let [cs (seq coll)]
      (let [item (first cs)]
        (if (contains? seen item)
          (recur seen (conj dups item) (rest cs))
          (recur (conj seen item) dups (rest cs))))
      dups)))

;; new reducers in 1.5-alpha1 are probably a better idea than this
(defn take-reduce
  "Like reduce but the first argument limits the number of items to take from the sequence for the reduction."
  ([n f s] 
    (take-reduce (dec n) f (first s) (rest s) ))
  ([n f initial s]
     (let [s (seq s)]
       (if (or (<= n 0) (nil? s))
         initial
         (recur (dec n) f (f initial (first s)) (rest s))))))

;; This is quite fast, even though it doubles the compares.
(defn palindrome? [s]
  (= s (clojure.string/reverse s)))


;; Usually, you want sequential? (for flattening nested sequences, lists, and vectors).
;; If you just want to deal with lists and lazy-seqs, use seq? (vectors will not be flattened).
;; If you want to flatten maps, sets, and all kinds of collections, you could use coll?, but that
;; seems like an unusual situation.

;; flat-seq is about twice as fast as clojure.core/flatten.
;; If you're going to use reduce on the result, see the new clojure.core.reducers library
;; in Clojure 1.5 alpha1 for a much faster way to flatten.

(defn flat-seq 
  "Like `clojure.core/flatten` but better, stronger, faster.  Takes an optional
   predicate `pred` that returns true if an element should be flattened.  If unspecified,
   the default pred is sequential?.  Returns a single, flat, lazy sequence.  If
   `x` is nil, nil is returned.  If `(pred x)` is falsey, returns `(list x)` so it's
   always safe to treat the result as a seq."
  {:static true}
  ([x] (flat-seq sequential? x))
  ([pred x] (letfn [(flat [coll] 
                      (lazy-seq 
                       (when-let [c (seq coll)] 
                         (let [x (first c)]
                           (if (pred x) 
                             (concat (flat x) (flat (rest c))) 
                             (conj (flat (rest c)) x))))))]
        (cond (nil? x) nil
              (pred x) (flat x)
              :else (list x)))))


(defn lazy? [x]
  (and (instance? clojure.lang.IPending x) (seq? x)))

(defn realize [x]
  (if (lazy? x) (doall x) x))

(defn range-down
  "Returns a seq of integers from high (exclusive) down to low (inclusive).
   Low defaults to 0. Step is a positve decrement, defaults to 1.  Like
   (reverse (range low high step)) but a bit faster."
  ([high] (range (dec high) -1 -1))
  ([high low] (range (dec high) (dec low) -1))
  ([high low step]
     ;; calculate nearest multiple of step + offset using mod
     (let [top (+ (- high (mod high step)) (mod low step))]
       (range (if (>= top high) (- top step) top) (dec low) (- step)))))

;; Something like this used to be in the fs lib, but it was dropped in v1.0.0.
(defn fs-join [& path-elements]
  "Joins path-elements into a string using the File/separator between elemements."
  (apply str (interpose java.io.File/separator path-elements)))


(defn ch-upper? [ch]
  (<= (int \A) (int ch) (int \Z)))

(defn ch-lower? [ch]
  (<= (int \a) (int ch) (int \z)))

(defn- ch-starter [ch]
  (cond (ch-upper? ch) \A
        (ch-lower? ch) \a
        :else (throw (java.lang.IllegalArgumentException.
                      (str "Expecting a character A-Z or a-z, but got: '" ch "'")))))

;; Note: end char is inclusive (because that's what I want!)
(defn ch-range
  ([end] (ch-range (ch-starter end) end 1))
  ([start end] (ch-range start end 1))  
  ([start end step]
     (map char (range (int start) (inc (int end)) step))))

(defn guard-fn
 "Returns a function taking a single argument and returns the result of applying f to the arg only
 if pred is satisfied by the arg.  Otherwise, the arg is returned."
 [pred f]
 (fn [arg] (if (pred arg) (f arg) arg)))


(defn concatv
  "Returns the concatenation of vectors."
  ([] []) 
  ([v] v) 
  ;; stolen from clojure.core/into, but assumes all vectors
  ([to from] (persistent! (reduce conj! (transient to) from)))
  ([to from from2] (persistent! (reduce conj! (reduce conj! (transient to) from) from2)))
  ;; maybe should use reducers instead of recursion
  ([to from from2 & more] (apply concatv (concatv to from from2) more)))

;; stolen from https://github.com/ptaoussanis/faraday
(defmacro doto-cond "Diabolical cross between `doto`, `cond->` and `as->`."
  [[name x] & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        pstep (fn [[test-expr step]] `(when-let [~name ~test-expr]
                                       (-> ~g ~step)))]
    `(let [~g ~x]
       ~@(map pstep (partition 2 clauses))
       ~g)))

(defn javaPrivateFieldValue 
  "Dangerous hack, don't use it"
  [obj-or-class fieldName]
  (let [c (class obj-or-class)
        obj (when-not (= c Class) obj-or-class)
        clazz (if (= c Class) obj-or-class c)
        ^java.lang.reflect.Field field  (try (.getDeclaredField ^Class clazz fieldName) (catch NoSuchFieldException e nil))]
    (when field
      (.setAccessible field true)
      (.get field obj))))

;; http://stackoverflow.com/questions/3407876/how-do-i-avoid-clojures-chunking-behavior-for-lazy-seqs-that-i-want-to-short-ci

(defn unchunk
  "Makes lazy seq from `s` that does not chunk values.  Might be useful for preventing
  excess computation or side-effects."
  [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (unchunk (next s))))))

(defn keep-first
  "Returns first truthy result of lazily applying `f` to each of the elements of `xs`.
  Returns nil if no truthy result is found.  Unlike `keep`, will not return false."
  [f xs]
  (first (remove false? (keep f xs))))

;; (case-of? expr foo bar) is better than (or (= expr 'foo) (= expr 'bar))
(defmacro case-of? 
  "Returns true if `expr` evaluates to any of the `constants`, otherwise false.
As with `case`, constants must be compile-time literals, and need not be quoted."
  [expr & constants]
  `(case ~expr
     ~constants true
     false))

;; source: http://stackoverflow.com/questions/1490869/how-to-get-vm-arguments-from-inside-of-java-application
(defn jvmOpts []
  "Returns seq of JVM options (strings)"
  (seq (.getInputArguments (java.lang.management.ManagementFactory/getRuntimeMXBean))))

(defn warn-on-suspicious-jvmopts []
  (when (some #(.startsWith ^String % "-XX:TieredStopAtLevel=") (jvmOpts))
    (println "Warning: TieredStopAtLevel setting may give bad benchmark results.  Check your Leiningen settings.  https://github.com/technomancy/leiningen/blob/stable/doc/FAQ.md")))

;; bench is a quick and dirty micro-benchmarking tool that realizes
;; the result (at the top level) in order to avoid misleading timings
;; due to laziness.  Use bench instead of clojure.core/time.
;; For serious work, use the criterium project, not this.
(defmacro bench [expr]
  `(let [result# (realize ~expr)]
     (warn-on-suspicious-jvmopts)
     ;; warm up
     (dotimes [n# 5] (realize ~expr))
     (dotimes [n# 5] (time (realize ~expr)))
     (binding [clojure.core/*print-level* 10
               clojure.core/*print-length* 10]
       (println result#))
     nil))

;; adapted from "guns" self@sungpae.com on the ML
(defmacro dump-locals []
 `(do (println  "; locals")
      (clojure.pprint/pprint
       ~(into {} (map (fn [x] [`'~x x]) (reverse (keys &env)))))))
