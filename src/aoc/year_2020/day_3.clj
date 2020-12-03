(ns aoc.year-2020.day-3
  (:require [clojure.java.io :as io]))

(defn read-input []
  (with-open [rdr (io/reader (io/resource "year_2020/input/day_3"))]
    (loop [acc {} i 0 [line & lines] (line-seq rdr)]
      (if line
        (recur (assoc acc i (cycle (map #(case % \. 0 \# 1) line))) (inc i) lines)
        acc))))

(defn solution [jj ii]
  (let [m (read-input)
        max-k (apply max (keys m))]
    (loop [acc 0 i 0 j 0]
      (if (= j max-k)
        acc
        (recur (+ acc (nth (get m (+ j jj)) (+ i ii)))
               (+ i ii)
               (+ j jj))))))

(defn solution-1 []
  (solution 1 3))

(defn solution-2 []
  (reduce *' (map #(apply solution %) [[1 1] [1 3] [1 5] [1 7] [2 1]])))

(comment

  (solution-1)

  (solution-2)

  )
