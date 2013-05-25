(ns intro)

;; Clojure sample code (list concatenation)
(defn concat [xs ys]
  (if (empty? xs) ys
      (let [[h & t] xs]
        (cons h (concat t ys)))))


;; Values

(def a 34)
(def x 2.0)
(def s "clojure")

(let [x 10 y 20]
  (+ x y))

;; Scope
(let [b 10]
  (println a b)
  (let [a 4]
    (println a b)
    (let [a 1 b 2]
      (println a b))))

(println a) ;; 34
;;(println b)

;; Local functions
(let [adder (fn [a b] (+ a b))]
  (println (adder 1 2)))

;; Private functions

;; Functions

(defn factorial [n]
  (if (zero? n) 1
      (* n (factorial (dec n)))))


;; Factorial 2

(defn factorial2 [n]
  (reduce *' (range 1 (inc n))))

;; Collections

;; List
(def aList (list 1 2 3))

;; Vector
(def aVec [1 2 3])

;; Map
(def aMap {:name "Chuck"
           :last "Norris"
           :kill 100})

;; Set
(def aSet #{:apple :orange :coffee})

;; Destructuring



;; Loop/Recur

(defn factorial [n]
  (loop [cur n acc 1N]
    (if (zero? cur) acc
        (recur (dec cur) (* acc cur)))))

;; Functional Features

;; Awesome Functions

;; Lazy Sequences

;; Meta
(def batman "Batman")
(def batman (with-meta 'batman {:bat true :human true :speed 100}))

;; Type Hints
(defn add [^long a ^long b]
  (+ a b))


;; Pre/Post

(defn increase-salary [e]
  {:pre [(< (:salary e) 100000)
         (not (:fired e))]}
  (update-in e [:salary] + 1000))


;; Records
(defrecord TreeNode [value left right])