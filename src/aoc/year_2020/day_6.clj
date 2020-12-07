(ns aoc.year-2020.day-6
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input [& [test?]]
  (with-open [rdr (io/reader (io/resource (str "year_2020/input/day_6" (when test? "_test"))))]
    (loop [groups [] group [] [line & lines] (line-seq rdr)]
      (if line
        (let [groups (if (string/blank? line)
                       (conj groups group)
                       groups)
              group (if (string/blank? line)
                      []
                      (conj group line))]
          (recur groups group lines))
        (conj groups group)))))

(defn solve-1 [groups]
  (reduce +
          (map (fn [group]
                 (count (distinct (mapcat identity group))))
               groups)))

(defn solve-2 [groups]
  (reduce +
          (map (fn [group]
                 (reduce-kv
                  (fn [acc k v]
                    (if (= v (count group))
                      (inc acc)
                      acc))
                  0
                  (frequencies (mapcat identity group))))
               groups)))

(comment

  (read-input true)

  (solve-1 (read-input))

  (solve-2 (read-input))

  )
