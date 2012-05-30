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
