(ns jug)

;; Value
(def a 42)
(type a)
(def pi 3.14)
(def planet "Earth")

;; Functions
(fn [a b] (+ a b))
((fn [a b] (+ a b)) 39 3)
#(+ %1 %2)
(#(+ %1 %2) 10 30)
(def square (fn [x] (* x x)))
(defn square [x] (* x x))

(defn abs-product [a b]
  (if (< (* a b) 0) 
    (- (* a b))
    (* a b)))

;; Scope
(let [a 10 b 20] (+ a b))

(let [a 10]
  (println a)
  (let [a 20]
    (println a))
  (println a))

(defn abs-product [a b]
  (let [p (* a b)]
    (if (< p 0) (- p) p)))

;; Collections

;; List
(1 2 3)
'(1 2 3)
(def a (list 1 2 3))
(first a)
(rest a)
(cons "JUG" a)
(count a)
(concat a (list 5 6 7))
(list 1 (list 2 (list 3 (list 4))))

;; Vector
[1 2 3]
(def a [1 2 3])
(first a)
(rest a)
(conj a 4)
(nth a 1)
;; No remove???
(subvec a 0 1)

;; How achieve?
(vec (concat (subvec a 0 1) (subvec a 2 3))) 

;; Update
(assoc a 0 100)
(update-in a [1] inc)

;; Map
{:age 24 :name "John"}
(def m {:age 24 :name "John"})
(get m :name)
(get m :age)
(get m :pro)
(get m :pro "Driver")

(assoc m :pro "Developer")
(dissoc m :name)

(keys m)
(vals m)

;; Set
#{1 2 3}
(conj a 1)
(disj a 2)
(contains? a 3)

;; Functional features
(map inc [1 2 3 4 5])
(filter even? [1 2 3 4 5])
(reduce + [1 2 3 4 5])
 ;; iterate
 ;; partial

;; Automatic Promotion
(range 10)
(range 1 10)
(reduce * (range 1 10))
(reduce * (range 1 100))
(reduce * 1N (range 1 100))
(reduce *' (range 1 100))

;; Destructuring (poor man's pattern matching)
(let [a [1 2 3 4]]
  [(first a)
   (nth a 2)])

(let [[a b c d] [1 2 3 4]]
  [a c])

(let [[a _ c _] [1 2 3 4]]
  [a c])

(let [[h & t] [1 2 3 4]]
  [h t])

;; Loop/Recur
(defn my-count [lst]
  (if (empty? lst) 0
      (+ 1 (my-count (rest lst)))))

(my-count [1 2 3])
(my-count (range 100))

(my-count (range 10000)) ;; StackOverflow

(defn my-count [lst]
  (loop [xs lst cnt 0]
    (if (empty? xs) cnt
        (recur (rest xs) (+ 1 cnt)))))
;; Time
(time (my-count (range 10000)))

;; Memoization

(defn greet [msg]
  (println "Hello" msg)
  (Thread/sleep 5000)
  (count msg))

(greet "JUG")
(def greet-memo (memoize greet))

(greet-memo "JUG")
(greet-memo "JUG")

;; Lazy Sequences

(range)
(take 10 (range))
(take 10 (drop 10 (range)))
(take-while #(< % 100) (filter even? (map square (range))))

;; Thread macro
;; Thread-Last
(->> (range)
     (map square)
     (filter even?)
     (take-while #(< % 100)))

;; Thread First
(-> 10
    (+ 2)
    (- 100)
    (/ 10)
    square
    str)

;; Thread Hack
(->> (range)
     (take 10)
     vec
     (#(conj % 1)))

;; Metainformation
(with-meta [1 2 3 4 5] {:message "This is an array"})
(def a (with-meta [1 2 3 4 5] {:message "This is an array"}))
(meta a)

;; IO
(slurp "/home/mishadoff/jug.txt")
(println (slurp "/home/mishadoff/jug.txt"))
(spit "/home/mishadoff/jug.txt"
      "Thanks for Attending"
      :append true)

(require 'clojure.java.io)
(with-open [r (reader "/home/mishadoff/jug.txt")]
  (doseq [line (line-seq r)]
    (println line)))

;; Regexps
(re-seq #"\w+" (slurp "/home/mishadoff/jug.txt"))

;; Record
(defrecord ProgLang [name year])
(def clj (new ProgLang "Clojure" 2008))
(keys clj)
(vals clj)

;; Protocol
(defprotocol Age
  (how-old? [this]))

(defprotocol Name
  (getName [this]))

(defrecord ProgLang [name year]
  Age Name
  (how-old? [this] (- 2013 year))
  (getName ^String [this] name))

(how-old? (new ProgLang "Clojure" 2008))
(getName (new ProgLang "Clojure" 2008))

(extend-protocol 
    Age java.lang.String
    (how-old? [this] "String are very old"))
(how-old? "Clojure")

;; Type Hints

(defrecord ProgLang [name year]
  Age Name
  (how-old? [this] (- 2013 year))
  (getName [this] (.toUpperCase name)))

;; time

(defrecord ProgLang [^String name year]
  Age Name
  (how-old? [this] (- 2013 year))
  (getName [this] (.toUpperCase name)))

;; Multimethods

;; Runtime Polymorhism

(defn rectangle [w h] 
  {:type :rectangle
   :w w
   :h h})

(defn circle [r]
  {:type :circle
   :r r})

(defmulti area :type)

(defmethod area :rectangle [r]
  (* (:w r) (:h r)))
(defmethod area :circle [r]
  (* 3.14 (:r r) (:r r))) 

;; Exceptions
(try
  (println "Hello")
  (println "World")
  (+ 1 2 3 4)
  (catch Exception e "ERROR"))

(throw (new Exception))

;; Macro

;; Java Interoperability (both)

;; Bonus
