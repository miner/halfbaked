

;; attributed to Rick Hickey
;; Used to "winnow" results, first item of vector contains elements of coll
;; that pred returns true for, second item has the false elements.
;; When timing, be careful about laziness and let the JIT work on it.
;; See also http://www.gettingclojure.com/cookbook:sequences#commas
(defn unzip-with [pred coll]
  (let [pvs (map #(vector (pred %) %) coll)]
    [(for [[p v] pvs :when p] v)
     (for [[p v] pvs :when (not p)] v)]))

;; Adrian Cuthbertson version -- also good after JIT
(defn winnow [pred coll]
  (reduce (fn [[a b] x]
            (if (pred x)
              [(conj a x) b]
              [a (conj b x)]))
          [[] []]
          coll))

;; My version -- slightly faster, but longer
(defn winn2  [pred coll]
  (let [winnow (fn [pred xs goods bads]
                 (if-let [xs (seq xs)]
                   (let [x (first xs)]
                     (if (pred x)
                       (recur pred (rest xs) (conj goods x) bads)
                       (recur pred (rest xs) goods (conj bads x))))
                   [goods bads]))]
   (winnow pred coll [] [])))

