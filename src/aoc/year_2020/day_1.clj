(ns aoc.year-2020.day-1)

(defn fix-input [input]
  (map-indexed vector input))

(defmacro solution [n input]
  (let [vars (for [_ (range n)] [(gensym) (gensym)])
        input-sym (gensym)]
    `(let [~input-sym (fix-input ~input)]
       (first (for [~@(mapcat (fn [var-sym]
                                [var-sym input-sym])
                              vars)
                    :when (not= ~@(map first vars))
                    :when (= 2020 (+ ~@(map second vars)))]
                (* ~@(map second vars)))))))
