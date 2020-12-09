(ns aoc.year-2020.day-9
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn read-input [& [test?]]
  (with-open [rdr (io/reader (io/resource (str "year_2020/input/day_9" (when test? "_test"))))]
    (mapv edn/read-string (line-seq rdr))))

(defn valid-number? [xs n]
  (first (for [x1 xs
               x2 xs
               :when (= n (+' x1 x2))]
           n)))

(defn solution-1 [preamble input]
  (reduce
   (fn [res n]
     (let [subxs (subvec input (- res preamble) res)]
       (if (valid-number? subxs n)
         (inc res)
         (reduced n))))
   preamble
   (drop preamble input)))

(defn solution-2 [n input]
  (reduce
   (fn [_ i]
     (loop [acc 0 i' i]
       (if-let [x (nth input i' nil)]
         (let [acc (+ acc x)]
           (cond
             (= acc n) (let [xs (subvec input i (inc i'))]
                         ;; NOTE return reduced value from the loop ;)
                         (reduced (+ (apply min xs) (apply max xs))))
             (< acc n) (recur acc (inc i'))
             :else nil))
         nil)))
   nil
   (range (count input))))

(comment

  (read-input true)

  (solution-1 5 *1)

  (solution-2 *1 *2)

  (read-input)

  (solution-1 25 *1)
  ;; => 675280050

  (solution-2 *1 *2)
  ;; => 96081673

  )
