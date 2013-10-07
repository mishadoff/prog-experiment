(ns sicp)

;; Chapter 1.1

;; - Structure of Expression
;; - Evaluation model
;; - Basic syntax rules
;; - Lexical scoping

;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3

(defn sum-of-square-max-two [a b c]
  (let [[p q] (reverse (sort [a b c]))]
    (+ (* p p) (* q q))))

;; Newton square-root

(defn sqrt-newton [n]
  (let [improve (fn [guess]
                  (/ (+ guess (/ n guess)) 2.0))
        good-enough? (fn [guess]
                       (let [a (- (* guess guess) n)]
                         (< (if (neg? a) (- a) a) 0.001)))
        sqrt-iter (fn [guess]
                    (if (good-enough? guess) 
                      guess
                      (recur (improve guess))))]
    (sqrt-iter 1.0)))
