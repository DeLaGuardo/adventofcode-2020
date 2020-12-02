(ns aoc.utils
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as reader-types]))

(defn read-puzzle-input [n]
  (let [rdr (reader-types/to-rdr (io/reader (io/resource (format "year_2020/input/day_%s.edn" n))))]
    (loop [acc [] rdr rdr]
      (let [item (edn/read {:eof ::eof} rdr)]
        (if (= item ::eof)
          acc
          (recur (conj acc item) rdr))))))
