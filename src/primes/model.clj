(ns primes.model)

; Returns the range of possible factors for a number.
(defn factor-range [num]
  (range 2 (inc (Math/sqrt num))))

; Returns true is factor divides evenly into num
(defn factor? [num factor]
  (= 0 (mod num factor)))

; Returns true if the number has other factors which means it's non-prime
(defn has-factors? [num]
  (first
    (for [factor (factor-range num) :when (factor? num factor)]
      true)))

; Returns true if the number is prime
(defn prime? [num]
  (cond
    (= num 0) false
    (= num 1) false
    (= num 2) true
    :else     (not (has-factors? num))))

; Returns a list of the requested number of primes
(defn list-primes [qty]
  (take qty
    (for [num (range) :when (prime? num)]
      num)))

; Returns a 2D list of the multiplication table of primes
(defn table [size]
  (let [primes (flatten (conj [nil] (list-primes size)))]
    (for [y (range (inc size))]
      (for [x (range (inc size))]
        (if (= 0 x y) nil
          (*
            (or (nth primes x) 1)
            (or (nth primes y) 1)))))))

; Generate and print the table
(defn -main []
  (doseq [row (table 10)]
    (doseq [num row]
      (print (str num "\t")))
    (println "")))
