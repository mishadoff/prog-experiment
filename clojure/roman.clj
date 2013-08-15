(ns roman)

(def to-roman-map
  (sorted-map-by > 1 "I" 4 "IV" 5 "V" 9 "IX" 10 "X"
                 40 "XL" 50 "L" 90 "XC" 100 "C"
                 400 "CD" 500 "D" 900 "CM" 1000 "M"))

(def from-roman-map {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn roman-closest [num]
  (first (drop-while #(> % num)(keys to-roman-map))))

(defn number->roman [num]
  ;; validate positive
  (letfn [(closest [n] 
            (->> (keys to-roman-map)
                 (drop-while #(> % n))
                 first))
          (roman-seq [n acc]
            (if (zero? n) acc
                (let [c (closest n)]
                  (recur (- n c) (conj acc c)))))]
    (apply str (map to-roman-map (roman-seq num [])))))

(defn roman->number [s]
  ;; input must be a valid roman string ;; regexp
  (letfn [(sum [a b]
            (if (< a b) (+ a b) (- a b)))]
    (->> (reverse s)
         (map from-roman-map)
         (partition-by identity)
         (map (partial reduce +))
         (reduce sum))))

(defn test []
  (for [i (range 1 100) :when (not= i (roman->number (number->roman i)))]
    i))
