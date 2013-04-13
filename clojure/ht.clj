(ns ht)

(defn flip []
  (if (zero? (rand-int 2)) :H :T))

(defn exp [pattern]
  (loop [i 0 prev3 (vec (repeat (count pattern) 0))]
    (if (= prev3 pattern) i
        (recur (inc i) (conj (vec (rest prev3)) (flip))))))

(defn average [pattern num]
  (loop [i 0 sum 0]
    (if (= i num) (double (/ sum num))
        (recur (inc i) (+ sum (exp pattern))))))

(defn run []
  (do
    (println "Experiment 1 (HTH) : " (average [:H :T :H] 1000000))
    (println "Experiment 2 (HTT) : " (average [:H :T :T] 1000000))))

;; Run experiment for 1M times
;; Experiment 1 (HTH) :  10.004874
;; Experiment 2 (HTT) :  8.001436