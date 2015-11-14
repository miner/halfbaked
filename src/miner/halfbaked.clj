(ns miner.halfbaked
  (:require [clojure.string :as str]))


(defn sym
  "Like `symbol` but allows more flexibility in the types of arguments, which can be a Namespace,
  String, Keyword or Symbol."
  ([x] (cond (symbol? x) x
             (string? x) (symbol x)
             (keyword? x) (symbol (namespace x) (name x))
             :else (symbol (str x))))
  ([ns x] (cond (nil? ns) (sym x)
                (string? ns) (symbol ns (name x))
                (or (keyword? ns) (symbol? ns)) (symbol (name ns) (name x))
                ;; (instance? clojure.lang.Namespace ns)
                :else (symbol (str ns) (name x)))))


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
        compiled-name (when (fn? f) (str f))
        fname (second (first (re-seq #"[$](.*)@" compiled-name)))]
    (if fname
      (demangle fname)
      compiled-name)))


;; original:
;; http://groups.google.com/group/clojure/browse_thread/thread/234ac3ff0a4b6b80?pli=1
;; but slightly changed for Clojure updates since 1.0

;; See also clojure.repl/demunge for the official way to do this now.

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

;; ray@1729.org.uk on clojure mailing list
(defn rotations [xs]
  "Returns a seq of all possible rotations of the finite seq `xs`"
  (let [n (count xs)]
    (take n (partition n 1 (cycle xs)))))

;; probably not so efficient since it walks twice
(defn kvs-map [fkey fval coll]
  (zipmap (map fkey coll) (map fval coll)))

;; probably better
(defn map-by [fkey fval coll]
  (into {} (map (juxt fkey fval) coll)))

;; consider reduce-kv as an alternative if you're going to reduce the result

;; maps f against success tails of the coll
(defn map-rest [f coll]
  (map f (take-while seq (iterate rest coll))))


;; Like standard interleave but doesn't drop excess elements; also works with zero or one
;; arg.  It's prettier without the two-arg variant, but that's the common case and this way
;; is faster than using just the variadic variant.

(defn interleave-all
  "Returns a lazy seq of the first item in each collection, then the second, etc.  If one 
collection ends, continues to interleave the others.  Naturally, you should take care with
infinite sequences."
  ([] (lazy-seq nil))
  ([c] (lazy-seq c))

  ([c1 c2]
     (lazy-seq
      (cond (not (seq c1)) c2
            (not (seq c2)) c1
            :else (conj (interleave-all (rest c1) (rest c2)) (first c2) (first c1)))))

  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (keep seq (conj colls c2 c1))]
        (concat (map first ss) (apply interleave-all (map rest ss)))))))


;; reference: http://www.rgagnon.com/javadetails/java-0456.html
(defn asciify [s]
  "Returns an ASCII string approximation of the string `s`"
  (let [normalized (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
        diacriticals #"\p{InCombiningDiacriticalMarks}+"]
    (clojure.string/replace normalized diacriticals "")))

;; works better than asciify for docs converted from MS Word or FileMaker
(defn str-clean [st]
  (str/escape (str/trim st) {\u2019 \' \u2018 \' \u201c \" \u201d \" \u2013 \- \u2014 \-}))

;; rem is slightly faster than mod, their zeroes are equivalent
(defn zmod? [num divisor]
  "Returns true iff `num` is a multiple of `divisor`"
  (zero? (rem num divisor)))

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
(defmacro defn0 [fname & fdcls]
  `(let [~'%0 (symbol (name (ns-name *ns*)) (name '~fname))]
     (defn ~fname ~@fdcls)))


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
;; for a much faster way to flatten.

;; guts of flat inspired by a post by Mark Engleberg  mark.engelberg@gmail.com
(defn flat-seq
  "Like `clojure.core/flatten` but better, stronger, faster.  Takes an optional
   predicate `pred` that returns true if an element should be flattened.  If unspecified,
   the default pred is sequential?.  Returns a single, flat, lazy sequence.  If
   `x` is nil, nil is returned.  If `(pred x)` is falsey, returns `(list x)` so it's
   always safe to treat the result as a seq."
  {:static true}
  ([xs] (flat-seq sequential? xs))
  ([pred xs]
     (let [flat1 (fn flat [pred xs]
                   (if-let [xs (seq xs)]
                     (let [y (first xs)
                           ys (rest xs)]
                       (if (pred y)
                         (if-let [y (seq y)]
                           (recur pred (cons (first y) (cons (rest y) ys)))
                           ;; empty list case
                           (recur pred ys))
                         (lazy-seq (cons y (flat pred ys)))))
                     ()))]
       (cond (nil? xs) nil
             (pred xs) (flat1 pred xs)
             :else (list xs)))))


;; Consider using reducers instead of this
(defn eager-flatten
  "not lazy, but much faster than clojure.core/flatten."
  [coll] 
  (loop [cs coll, result []]
    (cond (sequential? (first cs)) (recur (concat (first cs) (rest cs)) result)
          (empty? cs) (seq result)
          :else (recur (rest cs) (conj result (first cs))))))


(defn lazy? [x]
  (and (instance? clojure.lang.IPending x) (seq? x) (not (realized? x))))

(defn realize [x]
  (if (lazy? x) (doall x) x))

(defn range-down
  "Returns a seq of integers from HIGH (exclusive) down to LOW (inclusive).
   LOW defaults to 0. STEP is a positve decrement, defaults to 1.  Like
   `(reverse (range low high step))' but a bit faster."
  ([high] (range (dec high) -1 -1))
  ([high low] (range (dec high) (dec low) -1))
  ([high low step]
     ;; calculate nearest multiple of step + offset using mod
     (range (- (dec high) (mod (- (dec high) low) step)) (dec low) (- step))))

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


(defn catv
  "Returns the concatenation of the given collections as a vector."
  ([] []) 
  ([coll] (vec coll))
  ;; stolen from clojure.core/into, but assumes all vectors
  ([to from]
   (into (if (vector? to) to (vec to)) from))
  ([to from from2]
   (if (vector? to)
     (persistent! (reduce conj! (reduce conj! (transient to) from) from2))
     (persistent! (reduce conj! (reduce conj! (transient (vec to)) from) from2))))
  ;; maybe should use reducers instead of recursion
  ([to from from2 & more]
   (if (vector? to)
     (reduce into to (list* from from2 more))
     (reduce into [] (list* to from from2 more)))))


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
(defmacro bench
  ([expr]
   `(let [result# (realize ~expr)]
      (println '~expr) (flush)
      (warn-on-suspicious-jvmopts)
      ;; warm up
      (dotimes [n# 5] (realize ~expr))
      (dotimes [n# 10] (time (realize ~expr)) (flush))
      (flush)
      (binding [clojure.core/*print-level* 10
                clojure.core/*print-length* 10]
        (println result#)
        (flush))
      nil))
  ([expr & exprs]
   `(do
      (bench ~expr)
      (bench ~@exprs))))

;; adapted from "guns" self@sungpae.com on the ML
;; not sure about needing the reverse
(defmacro dump-locals [label]
 `(do (println  "; locals" ~label)
      (clojure.pprint/pprint
       ~(into {} (map (fn [x] [`'~x x]) (reverse (keys &env)))))))

;; http://stackoverflow.com/questions/11579804/clojure-reduce-with-three-parameters
;;
;; reduce-like with previous as part of state
;; use clever partitioning to match up previous items
;; (partition 2 1 coll)
;; but you have to think about the start with no previous

(defn reduce2
  "Like `reduce` but takes two elements at a time from the collection.  The function `f`
must take 3 args, the first being the accumulated result.  The initial value `init` is required."
  [f init coll] 
  (reduce (fn [r [a b]] (f r a b)) init (partition 2 coll)))

(defn assoc-when 
  "Like `assoc` but assocs into the map `m` only if `v` is truthy."
  ([m k v] (if v (assoc m k v) m))
  ([m k v & kvs] (reduce2 assoc-when
                          (assoc-when m k v)
                          kvs)))

;; http://stackoverflow.com/questions/15629622/dissoc-with-pred
(defn dissoc-by 
  "Dissoc map entries in `m` for which `(f k v)` returns logically true, which k and v are
  the key and value of the map entry."
  [f m]
  (reduce-kv (fn [m k v]
               (if (f k v)
                 m
                 (dissoc m k)))
             m m))

;; For small maps, it's much faster to use (shrink-map (hash-map ...)) than to use assoc-when
(defn shrink-map 
  "Remove falsey values and associated keys from the map `m`"
  [m]
  (reduce-kv (fn [r k v] (if-not v (dissoc r k) r)) m m))

(defn compatible? 
  "Returns true if two maps are 'compatible' in the sense that common keys have = values or
are nil.  That is, there are no conflicting values, ignoring nil."
  [m1 m2]
  (or (empty? m1)
      (empty? m2)
      (boolean (reduce-kv (fn [m k v] (cond (nil? v) m
                                            (nil? (get m k)) m
                                            (= (get m k) v) m
                                            :else (reduced false)))
                          m1
                          m2))))

;; from http://stackoverflow.com/questions/41107/how-to-generate-a-random-alpha-numeric-string
(defn rand-hex
  "Return a string of random hex digits of length `n' (default 16)"
  ([] (Long/toHexString (Double/doubleToLongBits (rand))))
  ([n] (str/join (conj (repeatedly (quot n 16) rand-hex)
                       (subs (rand-hex) (- 16 (rem n 16)))))))


;; The pr-str treatment works around problems with Clojure data that does not implement the
;; java.util.Formattable interface.  For example, a LazySeq doesn't look good in format %s.

(defn exception-format [fmt & args]
  "Like `format` but non-numeric args will be pre-converted with `pr-str` and thus present
  as strings.  That means only the numeric and string format codes are useful.  When in
  doubt, use %s.  `*print-level*` and `*print-length*` are constrained to avoid lengthy
  messages."
  (binding [*print-level* 2
            *print-length* 10]
    (apply format fmt (map (fn [x] (if (number? x) x (pr-str x))) args))))

(defn exception [info-map fmt & args]
  ;; ex-info reimagined
  ;; :cause value inside info-map is treated specially. As with ex-info, it must be Throwable.
  (if-let [cause (:cause info-map)]
    (ex-info (apply exception-format fmt args) (dissoc :cause info-map) cause)
    (ex-info (apply exception-format fmt args) info-map)))

;; hack to allow keywords as "guides" for arguments
;; (foo :a 1 :b 2)
;; (foo :a 1 2)
;; (foo 1 :b 2)
;;  all same as:
;; (foo 1 2)

;;; Really needs macro for def or call site to precompile into normal positional arg form
;;; Be aware of multi-arity calling.  Could also target single map arg.  (Just add the keys
;;; back in with zipmap.)

(defn dekey-args
  "Return a vector of args after removing optional 'guide' keywords.  The guide keywords, if
  present, must match order given by `keys`."  
  [keys all-args]
  (let [guides (set keys)]
    (loop [ks (seq keys) args (seq all-args) res []]
      (if (and ks args)
        (cond (= (first args) (first ks)) (recur ks (next args) res)
              (get guides (first args))
                (throw (exception {:keys keys :args all-args :unexpected (first args)}
                                  "Out of order guide keyword " (first args)))
              :else (recur (next ks) (next args) (conj res (first args))))
        (if args
          (throw (exception {:keys keys :args all-args :unmatched args}
                            "Unmatched args " args))
          res)))))


;; from declined CLJ-1468
(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level."
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))


(defn split= [marker coll]
  "Partitions items within coll as separated by marker value.  Marker never appears in results,
  and is ignored at the front or end of coll"
  (let [mark? (fn [x] (= x marker))]
    (take-nth 2 (partition-by mark? (drop-while mark? coll)))))


;; from Frank on mailing list
(defn partition-when
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           run (cons fst (take-while #(not (f %)) (next s)))]
       (cons run (partition-when f (seq (drop (count run) s))))))))



;; Note: for small vectors, linear search is probably just as good (using Java .indexOf)

;; Best official way would be to use a record like (defrecord Node [val prev next]) but there's
;; some syntax overhead and boilerplate code associate with that.  Finger-trees are another
;; possiblity.

;; When you want the equivalent of a doubly-linked list, use two maps
;; Variants return nil on boundaries, wrap or bound (overflow stays the same)
(defn prev-map [coll]
  (zipmap (rest coll) coll))

(defn next-map [coll]
  (zipmap coll (rest coll)))

(defn vlast [coll]
  ;; somewhat better than last for vector case
  (if (vector? coll) (peek coll) (last coll)))

;; If you want the next/prev to wrap around
(defn wrap-prev-map [coll]
  (zipmap coll (cons (vlast coll) coll)))

(defn wrap-next-map [coll]
  (zipmap (cons (vlast coll) coll) coll))

(defn bounded-prev-map [coll]
  (zipmap coll (cons (first coll) coll)))

(defn bounded-next-map [coll]
  (let [lst (vlast coll)]
    (zipmap (cons lst coll) (cons lst (rest coll)))))


;; app - helper for handling variadic functions in transducers...
;;   (map (app max))
;;
;; looks a bit nicer than any of these:
;;   (map #(apply max %))
;;   (map (partial apply max))
;;   (map (fn [args] (apply max args)))

;; "app" is shortened "apply", somewhat mnemonic.  Looks good in a transducer sequence.

(defn app
  "Similar to `partial`, but for use a variadic function.  Takes variadic `f` and any number
  of fixed arguments.  Returns a function that takes a collection as its single argument and
  applies `f` to the concatenation of the fixed arguments and the collection argument."
  ([f] (fn [args] (apply f args)))
  ([f a] (fn [args] (apply f a args)))
  ([f a b] (fn [args] (apply f a b args)))
  ([f a b c] (fn [args] (apply f a b c args)))
  ([f a b c & more] (fn [args] (apply f a b c (concat more args)))))


;; CLJ-1771 for multi-arity assoc-in
(defn assoc-in+
  ([m [k & ks] v]
   (if ks
     (assoc m k (assoc-in (get m k) ks v))
     (assoc m k v))) 
  ([m ks v & kvs]
   (let [ret (assoc-in m ks v)]
     (if kvs
       (if (next kvs)
         (recur ret (first kvs) (second kvs) (nnext kvs))
         (throw (ex-info "missing value" {:key (first kvs)})))
       ret))))


;; invented lastv at one point, but realized it was just peek

;; Related to power-limit
;; http://gearon.blogspot.com/2015/02/tweaking-power-limit.html

(defn fixed-point
  ([f] (fixed-point f 0))
  ([f guess] (fixed-point f guess 1000))
  ([f guess limit] (fixed-point f guess limit =))
  ([f guess limit eq?]
   (when (pos? limit)
     (let [guess' (f guess)]
       (if (eq? guess' guess)
         guess
         (recur f guess' (dec limit) eq?))))))

;; SEM rewrite with converge-seq (below)
(defn converge-seq-or-throw
  "Eager iteration of f, starting with n.  Terminates when result is repeated
  consecutively (reaches a fixed point).  Equality check calls eq? (default =).  Aborts if
  limit on count is reached (default 1000)."
  ([f n] (converge-seq-or-throw f n 1000 = []))
  ([f n limit] (converge-seq-or-throw f n limit = []))
  ([f n eq? limit] (converge-seq-or-throw f n limit eq? []))
  ([f n limit eq? res]
   (if (eq? (peek res) n)
     res
     (if (>= (count res) limit)
       ;; abort
       (throw (ex-info (str "Aborted converge-seq-or-throw due to result exceeding count limit " limit)
                       {:limit limit
                        :n n
                        :f (str f)
                        :partial-result res
                        :eq? (str eq?)}))
       (recur f (f n) limit eq? (conj res n))))))

;; lazy, but slow
(defn conv-seq
  ([f x] (conv-seq f x 1000))
  ([f x limit]
   (dedupe (take limit (iterate f x)))))

;; Does not expect nil as a starting X -- should be fixed now???
(defn converge-seq
  "Eager iteration of f, starting with x.  Terminates when result is repeated
  consecutively (reaches a fixed point).  Equality check calls eq? (default =).  Returns nil
  if limit count is exceeded (default 1000)."
  ([f x] (converge-seq f x 1000 =))
  ([f x limit] (converge-seq f x limit =))
  ([f x limit eq?]
   (let [conv (fn conv [^long limit res x]
                (if (eq? (peek res) x)
                  res
                  (when (pos? limit)
                    (recur (dec limit) (conj res x) (f x)))))]
     (conv x limit []))))

;; short for "keep if..."
(defn kif
  "Returns a new function that calls `testfn` on single argument and returns that argument
  if the test succeeds, otherwise returns result of optional `elsefn` on argument.  If `elsefn` is
  omitted, failure on `testfn` returns nil."
  ([testfn] (fn [x] (when (testfn x) x)))
  ([testfn elsefn] (fn [x] (if (testfn x) x (elsefn x)))))

;; potential issue with multiple evaluaton of ~f, but that is expected to be a symbol
(defmacro unroll1 [f base & args]
  (if args
    `(unroll1 ~f (~f ~base ~(first args)) ~@(next args))
    base))

(defmacro unroll2 [f base & args]
  (if args
    (if (next args)
      `(unroll2 ~f (~f ~base ~(first args) ~(second args)) ~@(nnext args))
      (throw (ex-info "Missing value" {:last-seen (first args)})))
    base))



(defn peek-sorted [sorted]
  (first sorted))

;; should we throw on empty?
(defn pop-sorted [sorted]
  {:pre [(seq sorted)]}
  (disj sorted (peek-sorted sorted)))

