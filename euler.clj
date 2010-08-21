(defn euler-2 []
  (letfn [(fib
           []
           (take-while)
           (map first
                (iterate
                 (fn [[a b]] [b (+ a b)])
                 [0 1])))]))