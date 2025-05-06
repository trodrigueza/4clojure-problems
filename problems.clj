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

;; Problem 42
(defn factorial42 [n] (reduce * 1 (take n (iterate inc 1))))

(defn factorial42v2 [n] (reduce *' 1 (range 1 (inc n))))

;; Problem 43
(defn revinter43 [v n] (apply map vector (partition-all n v)))

;; Problem 44
(defn rotate44 [n v]
  (let [seq (range 1 (inc (count v)))
        dict (apply hash-map (mapcat list v seq))
        dict-rev (apply hash-map (mapcat list seq v))
        newv (map dict v)]
    (map dict-rev
         (reduce
          (fn [res x]
            (let [tmp (mod (+ x n) (count v))]
              (if (= 0 tmp)
                (conj res (count v))
                (conj res tmp))))
          [] newv))))

(defn rotate44v2 [n v]
  (let [n (mod n (count v))
        ap (drop n v)]
    (concat ap (take n v))))

;; Problem 46
(defn flip46 [f]
  (fn [a b]
    (f b a)))

;; Problem 49 - Restriction: split-at
(defn split49 [n v]
  (conj [] (take n v) (drop n v))) 

;; Problem 50
(defn split-type50 [v]
  (vals (reduce (fn [res x]
                  (conj res [(type x) (concat (get res (type x) (list)) (list x))]))
               {} v)))

;; Problem 53
(defn longest-inc-seq [coll]
  (reduce
   (fn [res x]
     (if (and (> (count x) 1) (> (count x) (count res)))
       x res)) []
   (let [[a b] (reduce
               (fn [[res cur] x]
                 (if (empty? cur)
                   [res [x]]
                   (if (> x (last cur))
                     [res (conj cur x)]
                     [(conj res cur) [x]])))
               [[] []] coll)] (conj a b))))

;; Problem 54
(defn partition54 [n coll]
  (let [[res cur]
        (reduce
         (fn [[res cur] x]
           (if (= n (count cur))
             [(conj res cur) [x]]
             [res (conj cur x)]))
         [[] []] coll)]
    (if (= n (count cur))
      (conj res cur)
      res)))

;; Problem 55
(defn count-occ [coll]
  (reduce (fn [res x]
            (conj res [x (inc (get res x 0))]))
          {}
          coll))
(defn count-occv2 [coll]
  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} coll))

(defn count-occv3 [coll]
  (reduce (fn [res x] (assoc res x (inc (res x 0)))) {} coll))

;; Problem 56
(defn dist56 [coll]
  (reduce
   (fn [res x]
     (if (some #(= % x) res)
       res
       (conj res x)))
   [] coll))

;; Problem 58
(defn comp58
  ([a]
   (fn f1 [& args] (apply a args)))
  ([a b]
   (fn f2 [& args] (a (apply b args))))
  ([a b c]
   (fn f3 [& args] (a (b (apply c args))))))

(defn comp58v2
  [& fs]
  (reduce
   (fn [f g]
     (fn [& args]
       (f (apply g args))))
   fs))

