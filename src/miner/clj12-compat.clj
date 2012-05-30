
;; Andy Fingerhunt
;; https://github.com/jafingerhut/clojure-benchmarks/blob/master/pidigits/pidigits.clojure-2.clj
;; for Clojure 1.2 + 1.3 compatibility

;; was when-available
(defmacro if-class-available [classNameSymbol if-expr else-expr]
  (try
    (Class/forName (str classNameSymbol))
    if-expr
    (catch ClassNotFoundException _
      else-expr)))


;; was fixup
;; 'fixup' allows this code to work on both Clojure 1.3, which has the
;; new class clojure.lang.BigInt, and Clojure 1.2, which does not.

(defmacro asBigInt [j]
  (if-class-available
   clojure.lang.BigInt
   `(let [j# ~j]
      (if (instance? clojure.lang.BigInt j#)
        (.toBigInteger j#)
        j#))
   j))

;;; Bignum 1.2/1.3 compatibility
;;; Clever hack by Alan Malloy -- takes advantage of reader changes between versions to give you equivalent autopromotion addition.
(def +M (first [+' 1]))

