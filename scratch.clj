(defn min-1 [x & more]
  (loop [min x
         more (seq more)]
    (if-let [x (first more)]
      (recur (if (< x min) x min) (rest more))
      min)))

(defn min-2 [min & [x & more]]
  (if (nil? x)
    min
    (recur (if (< x min) x min) more)))

(defn min-3 [& coll]
  (reduce #(if (< %1 %2) %1 %2) coll))

(defn zipm-1 [keys vals]
  (loop [m {}
         ks keys
         vs vals]
    (if (and ks vs)
      (recur 
        (assoc m (first ks) (first vs))
        (next ks)
        (next vs))
      m)))

(defn zipm-2 [keys vals]
  (loop [m {}
         [key & ks] keys
         [val & vs] vals]
    (if (and key val)
      (recur (assoc m key val) ks vs)
      m)))

(defn zipm-3 [keys vals]
  (reduce
    (fn [m [k v]]
      (assoc m k v))
    {}
    (map vector keys vals)))

(defn zipm-4 [keys vals]
  (apply hash-map (interleave keys vals)))

(defn zipm-5 [keys vals]
  (into {} (map vector keys vals)))

(defn minmax-1 [x & more]
  (loop [min x
         max x
         [x & more] (seq more)]
    (if x
      (recur 
        (if (< x min) x min)
        (if (> x max) x max)
        more)
      [min max])))

(defn minmax-2 [x & coll]
  (reduce
    (fn [mm x]
      (let [min (:min mm)
            max (:max mm)]
        {:min (if (< x min) x min)
         :max (if (> x max) x max)}))
    {:min x :max x}
    coll))

(defn divides? [dividend divisor]
  (zero? (rem dividend divisor)))

(defn divides-any? [& nums]
  (fn [arg]
    (boolean (some #(divides? arg %) nums))))

(defn euler-1 [upper]
  (let [divisible? (divides-any? 3 5)]
    (loop [sum 0 n 1]
      (if (>= n upper) 
        sum
        (recur 
          (if (divisible? n) (+ sum n) sum) 
          (inc n))))))

(defn euler-1-left-to-right []
  (->> (range 1000) (filter (divides-any? 3 5)) (apply +)))

(defn euler-1-reduc
  ([] (euler-1-reduc 1000))
  ([upper]
     (apply
      +
      (filter
       (divides-any? 3 5)
       (range upper)))))

(defn euler-2 []
  (let [fibs (iterate #([%1]))]))