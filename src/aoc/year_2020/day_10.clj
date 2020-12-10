(ns aoc.year-2020.day-10
  (:require [clojure.java.io :as io]))

(defn read-input [& [test?]]
  (with-open [rdr (io/reader (io/resource (str "year_2020/input/day_10" (when test? "_test"))))]
    (mapv #(Long/parseLong %) (line-seq rdr))))

(defn solution-1
  ([input] (solution-1 0 {} (sort input)))
  ([current differences [adapter & adapters]]
   (if adapter
     (recur adapter (update differences (- adapter current) (fnil inc 0)) adapters)
     (let [{one 1 three 3} (update differences 3 (fnil inc 0))]
       (* one three)))))

(declare m-solution-2)

(defn solution-2* [[x1 x2 x3 x4 & _ :as input]]
  (if (and x1 x2)
    (+ (if (and x4 (<= (- x4 x1) 3))
         (m-solution-2 (drop 3 input))
         0)
       (if (and x3 (<= (- x3 x1) 3))
         (m-solution-2 (drop 2 input))
         0)
       (if (<= (- x2 x1) 3)
         (m-solution-2 (drop 1 input))
         0))
    1))

(def m-solution-2 (memoize solution-2*))

(defn solution-2 [input]
  (let [m (apply max input)
        input (sort (conj input 0 (+ m 3)))]
    (solution-2* input)))

(comment

  (def test-input '(16
                    10
                    15
                    5
                    1
                    11
                    7
                    19
                    6
                    12
                    4))

  (solution-1 (read-input true))
  ;; => 220

  (solution-1 (read-input))
  ;; => 2310

  (solution-2 test-input)
  ;; => 8

  (time
   (solution-2 (read-input true)))
  ;; => 19208

  (time
   (solution-2 (read-input)))
  ;; => 64793042714624

  )
