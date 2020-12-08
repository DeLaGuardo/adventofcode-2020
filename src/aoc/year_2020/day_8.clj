(ns aoc.year-2020.day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-instruction [line]
  (let [[command argument] (string/split line #" ")]
    {:cmd (keyword command)
     :args [(Long/parseLong argument)]}))

(defn read-input [& [test?]]
  (with-open [rdr (io/reader (io/resource (str "year_2020/input/day_8" (when test? "_test"))))]
    (loop [insts [] [line & lines] (line-seq rdr)]
      (if line
        (recur (conj insts (parse-instruction line)) lines)
        insts))))

(defmulti run-cmd (fn [_state {cmd :cmd}] cmd))

(defmethod run-cmd :acc [state {[addition] :args}]
  (-> state
      (update :acc + addition)
      (update :pointer inc)))

(defmethod run-cmd :jmp [state {[shift] :args}]
  (update state :pointer + shift))

(defmethod run-cmd :nop [state _]
  (update state :pointer inc))

(defn run-program
  ([program] (run-program {:pointer 0 :acc 0 :visited #{}} program))
  ([{:keys [pointer visited] :as state} program]
   (cond
     (= pointer (count program))
     (-> state
         (assoc :exit-code :ok)
         (dissoc :visited))

     (contains? visited pointer)
     (-> state
         (assoc :exit-code :loop)
         (dissoc :visited))

     :else
     (let [cmd (nth program pointer)]
       (recur (run-cmd (update state :visited conj pointer) cmd) program)))))

(defn fix-program [program]
  (for [i (range (count program))
        :let [{:keys [cmd args]} (nth program i)]
        :when (or (and (= cmd :nop) (not= (first args) 0))
                  (= cmd :jmp))
        :let [{:keys [exit-code] :as state}
              (run-program (update-in program [i :cmd]
                                      (fn [c] (case c :nop :jmp :jmp :nop))))]
        :when (= exit-code :ok)]
    state))

(comment

  (run-program (read-input))

  (fix-program (read-input))

  )
