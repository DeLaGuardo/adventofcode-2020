(ns aoc.year-2020.day-2
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn check-password-1 [password min max ch]
  (first (reduce
          (fn [[valid? counter] ch']
            (if (= ch ch')
              [(<= min (inc counter) max) (inc counter)]
              [valid? counter]))
          [false 0]
          password)))

(defn check-password-2 [password l r ch]
  (let [chl (nth password (dec l) nil)
        chr (nth password (dec r) nil)]
    (not= (= chl ch) (= chr ch))))

(defn read-input [f checker]
  (with-open [rdr (io/reader f)]
    (let [lines (line-seq rdr)]
      (loop [counter 0 [line & lines] lines]
        (if line
          (let [[_ min-s max-s ch password] (re-matches #"(\d+)-(\d+) (\w): (.*)$" line)
                valid? (checker password (edn/read-string min-s) (edn/read-string max-s) (first ch))]
            (recur ((if valid? inc identity) counter) lines))
          counter)))))

(comment

  (read-input (io/resource "year_2020/input/day_2") check-password-1)

  (read-input (io/resource "year_2020/input/day_2") check-password-2)

  (check-password-2 "abcde" 1 3 \a)

  )
