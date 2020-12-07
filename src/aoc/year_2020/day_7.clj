(ns aoc.year-2020.day-7
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input [& [test]]
  (with-open [rdr (io/reader (io/resource (str "year_2020/input/day_7" (when test "_test"))))]
    (loop [rules [] [line & lines] (line-seq rdr)]
      (if line
        (recur (conj rules line) lines)
        rules))))

(defn parse-rule [line]
  (let [[bag contains] (string/split line #" bags contain ")
        contains (into {} (map (fn [c]
                                 (when (not= c "no other bags.")
                                   (let [[_ c bag] (re-matches #"^(\d+) (\w+ \w+) bags?\.?$" c)]
                                     [bag (Long/parseLong c)]))))
                       (string/split contains #"(, )"))]
    [bag contains]))

(defn might-contain-bag? [rules k bag]
  (letfn [(step [k]
            (let [content (get rules k)]
              (reduce
               (fn [res k]
                 (if (or (= bag k) (contains? (get rules k) bag))
                   (reduced true)
                   (step k)))
               false
               (keys content))))]
    (step k)))

(defn find-for-key [rules k bag]
  (or (contains? (get rules k) bag)
      (reduce
       (fn [res k]
         (if-let [r (find-for-key rules k bag)]
           (reduced true)
           res))
       false
       (keys (get rules k)))))

(defn solution-1 [input]
  (let [rules (into {} (map parse-rule) input)]
    (reduce-kv
     (fn [acc k _]
       (if (find-for-key rules k "shiny gold")
         (inc acc)
         acc))
     0
     rules)))

(defn key-contains [rules k]
  (let [content (get rules k)]
    (if (not-empty content)
      (reduce + (map (fn [[k v]]
                       (* v (inc (key-contains rules k))))
                     (get rules k)))
      0)))

(defn solution-2 [input]
  (let [rules (into {} (map parse-rule) input)]
    (key-contains rules "shiny gold")))
