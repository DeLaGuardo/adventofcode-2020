(ns aoc.year-2020.day-3
  (:require [clojure.java.io :as io]))

(defn read-input []
  (with-open [rdr (io/reader (io/resource "year_2020/input/day_3"))]
    (loop [acc {} i 0 [line & lines] (line-seq rdr)]
      (if line
        (recur (assoc acc i (vec (map #(case % \. 0 \# 1) line))) (inc i) lines)
        acc))))

(defn solution [jj ii]
  (let [m (read-input)
        max-j (apply max (keys m))
        max-i (count (get m 0))]
    (reduce +' (map (fn [i j]
                      (get-in m [j (mod i max-i)]))
                    (iterate #(+ % ii) 0)
                    (range 0 (inc max-j) jj)))))

(defn solution-1 []
  (solution 1 3))

(defn solution-2 []
  (reduce *' (map #(apply solution %) [[1 1] [1 3] [1 5] [1 7] [2 1]])))

(comment

  (read-input)

  (time
   (dotimes [_ 100]
     (solution-1)))

  (time
   (dotimes [_ 100]
     (solution-2)))

  )
