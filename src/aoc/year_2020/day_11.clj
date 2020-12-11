(ns aoc.year-2020.day-11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input [& [test?]]
  (with-open [rdr (io/reader (io/resource (str "year_2020/input/day_11" (when test? test?))))]
    (reduce
     (fn [{:keys [max-i max-j] :as acc} [[i j :as index] ch]]
       (cond-> acc
         (= ch \L) (assoc index :empty)
         (= ch \#) (assoc index :occupied)
         (> i max-i) (assoc :max-i i)
         (> j max-j) (assoc :max-j j)))
     {:max-i 0
      :max-j 0}
     (vec
      (apply concat
             (map-indexed
              (fn [i line]
                (map-indexed
                 (fn [j ch]
                   [[i j] ch])
                 line))
              (line-seq rdr)))))))

(defn get-adjacent-indexes [i j {:keys [max-i max-j] :as input}]
  (for [i' (range (dec i) (+ i 2))
        j' (range (dec j) (+ j 2))
        :when (not= [i j] [i' j'])
        :when (<= 0 i' max-i)
        :when (<= 0 j' max-j)
        :when (contains? input [i' j'])]
    [i' j']))

(defn get-visible-indexes [i j {:keys [max-i max-j] :as input}]
  (apply concat (for [dir-i [inc dec identity]
                      dir-j [inc dec identity]
                      :when (not= dir-i dir-j identity)]
                  (loop [[i' & is] (iterate dir-i (dir-i i))
                         [j' & js] (iterate dir-j (dir-j j))]
                    (if (and (<= 0 i' max-i)
                             (<= 0 j' max-j))
                      (if (contains? input [i' j'])
                        [[i' j']]
                        (recur is js))
                      [])))))

(defn empty->occupied? [index adjacent input]
  (and (= :empty (get input index))
       (every? (fn [index]
                 (= :empty (get input index)))
               adjacent)))

(defn occupied->empty? [tolerance index adjacent input]
  (and (= :occupied (get input index))
       (<= tolerance (count (filter #(= :occupied (get input %)) adjacent)))))

(defn print-seats [{:keys [max-i max-j] :as input}]
  (println (string/join "\n" (for [i (range (inc max-i))]
                               (apply str (for [j (range (inc max-j))]
                                            (case (get input [i j])
                                              :empty \L
                                              :occupied \#
                                              \.)))))))

(defn step [get-indices-fn tolerance {:keys [max-i max-j] :as input}]
  (into input
        (for [i (range (inc max-i))
              j (range (inc max-j))
              :when (contains? input [i j])
              :let [adjacent (get-indices-fn i j input)]]
          (cond
            (empty->occupied? [i j] adjacent input) [[i j] :occupied]
            (occupied->empty? tolerance [i j] adjacent input) [[i j] :empty]
            :else [[i j] (get input [i j])]))))

(defn solution [input get-indices-fn tolerance & [with-print?]]
  (loop [counter 0 input input]
    (when with-print? (println (str "=== " counter " ===")))
    (when with-print? (print-seats input))
    (let [input' (step get-indices-fn tolerance input)]
      (if (= input input')
        [counter (count (filter #(= :occupied (second %)) input))]
        (recur (inc counter) input')))))

(comment

  (def test-input (read-input "_test"))

  (def input (read-input))

  (solution test-input get-adjacent-indexes 4 true)
  ;; => [5 37]

  (solution test-input get-visible-indexes 5 true)
  ;; => [6 26]

  (solution input get-adjacent-indexes 4)
  ;; => [85 2321]

  (solution input get-visible-indexes 5)
  ;; => [86 2102]

  )
