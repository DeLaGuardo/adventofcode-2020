(ns aoc.year-2020.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-passport
  ([st] (parse-passport {} st))
  ([acc st]
   (reduce
    (fn [acc st]
      (assoc acc (subs st 0 3) (subs st 4)))
    acc
    (string/split st #" "))))

(defn valid-byr? [byr]
  (and (= 4 (count byr))
       (try (<= 1920 (Integer/parseInt byr) 2002)
            (catch Exception _ false))))

(defn valid-iyr? [iyr]
  (and (= 4 (count iyr))
       (try (<= 2010 (Integer/parseInt iyr) 2020)
            (catch Exception _ false))))

(defn valid-eyr? [eyr]
  (and (= 4 (count eyr))
       (try (<= 2020 (Integer/parseInt eyr) 2030)
            (catch Exception _ false))))

(defn valid-hgt? [hgt]
  (when-let [[_ n c] (re-matches #"(\d+)(cm|in)" hgt)]
    (when-let [n (try (Integer/parseInt n) (catch Exception _ nil))]
      (case c
        "cm" (<= 150 n 193)
        "in" (<= 59 n 76)
        false))))

(defn valid-hcl? [hcl]
  (re-matches #"#[0-9a-f]{6}" hcl))

(defn valid-ecl? [ecl]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(defn valid-pid? [pid]
  (re-matches #"\d{9}$" pid))

(def validators
  {"byr" valid-byr?
   "iyr" valid-iyr?
   "eyr" valid-eyr?
   "hgt" valid-hgt?
   "hcl" valid-hcl?
   "ecl" valid-ecl?
   "pid" valid-pid?})

(defn valid? [p]
  (let [d (set/difference #{"byr"
                            "iyr"
                            "eyr"
                            "hgt"
                            "hcl"
                            "ecl"
                            "pid"
                            "cid"}
                          (set (keys p)))]
    (and (or (empty? d) (= #{"cid"} d))
         (every? (fn [[k f]]
                   (f (get p k)))
                 validators))))

(defn read-input []
  (with-open [rdr (io/reader (io/resource "year_2020/input/day_4"))]
    (loop [items [] item [] [line & lines] (line-seq rdr)]
      (if line
        (if (string/blank? line)
          (recur (conj items (string/join " " item)) [] lines)
          (recur items (conj item line) lines))
        (count (filter #(valid? (parse-passport %))
                       (conj items (string/join " " item))))))))

(comment

  (read-input)

  )
