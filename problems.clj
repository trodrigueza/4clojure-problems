;; Problem 34
(defn range34 [a b] (take (- b a) (iterate inc a)))

;; Problem 38
(defn max38 [& args] (reduce (fn [m a] (if (> a m) a m)) args))

;; Problem 39
(defn interleave39 [a b] (mapcat list a b))

;; Problem 40
(defn interpose40 [a v]
  (take (dec (* 2 (count v)))
        (reduce (fn [r x] (conj r x a))
                []
                v)))

(defn interpose40v2 [a v]
  (pop (reduce (fn [r x] (conj r x a)) [] v)))

;; Problem 41
(defn dropn41 [v n]
  (loop [i 0 res []]
    (if (>= i (count v))
      res
      (if (= 0 (mod (inc i) n))
        (recur (inc i) res)
        (recur (inc i) (conj res (get v i)))))))

(defn dropn41v2 [v n]
  (first
   (reduce (fn [[res i] x]
             (if (= 0 (mod i n))
               [res (inc i)]
               [(conj res x) (inc i)]))
           [[] 1]
           v)))

(defn drop41v3 [v n]
  (flatten (partition-all (dec n) n v)))


