(ns aoc.year-2020.day-2-test
  (:require [clojure.test :refer [deftest is]]
            [aoc.year-2020.day-1 :as sut]
            [aoc.utils :as utils]))

(def test-input
  [1721
   979
   366
   299
   675
   1456])

(def input
  (utils/read-puzzle-input 1))

(deftest sample-puzzle-1
  (is (= 514579 (sut/solution 2 test-input))))

(deftest sample-puzzle-2
  (is (= 241861950 (sut/solution 3 test-input))))

(deftest puzzle-1
  (is (= 805731 (sut/solution 2 input))))

(deftest puzzle-2
  (is (= 192684960 (sut/solution 3 input))))
