(ns problem1)

; Add all the natural numbers below one thousand that are multiples of 3 or 5.

(defn multiple-of-3-or-5? [x]
  (or
   (= (rem x 3) 0)
   (= (rem x 5) 0)))

(defn problem1 []
  (reduce +
          (filter multiple-of-3-or-5? (range 0 1000)))) 

