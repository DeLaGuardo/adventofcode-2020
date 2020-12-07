(ns aoc.year-2020.day-5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn seq-> [sx]
  (let [c (count sx)]
    (subvec sx (/ c 2))))

(defn <-seq [sx]
  (let [c (count sx)]
    (subvec sx 0 (/ c 2))))

(defn ->row [chs]
  (loop [x (vec (range 128)) [ch & chs] chs]
    (case ch
      \F (recur (<-seq x) chs)
      \B (recur (seq-> x) chs)
      x)))

(defn ->col [chs]
  (loop [x (vec (range 8)) [ch & chs] chs]
    (case ch
      \L (recur (<-seq x) chs)
      \R (recur (seq-> x) chs)
      x)))

(defn line->id [line]
  (let [row (->row (take 7 line))
        col (->col (drop 7 line))]
    (+ (* 8 (first row)) (first col))))

(defn read-input []
  (with-open [rdr (io/reader (io/resource "year_2020/input/day_5"))]
    (loop [ids [] [line & lines] (line-seq rdr)]
      (if line
        (recur (conj ids (line->id line)) lines)
        ids))))

(defn solve-1 [ids]
  (apply max ids))

(defn solve-2 [ids]
  (let [mi (apply min ids)
        mx (apply max ids)]
    (set/difference (set (range mi mx))
                    (set ids))))

(comment

  (solve-2 (read-input))

  (line->id "BFFFBBFRRR")

  (line->id "FFFBBBFRRR")

  (line->id "BBFFBBFRLL")

  )
