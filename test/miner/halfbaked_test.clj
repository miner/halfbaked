(ns miner.halfbaked-test
  (:use clojure.test
        miner.halfbaked))

(deftest range-down-test
  (are [top low step] (= (reverse (range low top step)) (range-down top low step))
       200 10 3
       199 11 2
       10 3 1
       11 2 1
       11 2 3
       120 51 49
       10 1 3
       11 1 3
       12 1 3
       13 2 3
       13 0 1
       13 1 3))

(deftest interleaving-all
  (is (= (interleave-all '(1 3 5) [2 4 6 7]) (range 1 8)))
  (is (= (interleave-all (range 3)) (range 3)))
  ;; regular interleave terminates on shortest collection
  (is (= (interleave '(1 4 7) '(2 5) [3 6 8 9]) (range 1 7)))
  (is (= (interleave-all [0] '(1 4 7) '(2 5) [3 6 8 9 10] ()) (range 11)))
  (is (= (interleave-all '(1 4 7) '(2 5) [3 6 8 9]) (range 1 10))))


