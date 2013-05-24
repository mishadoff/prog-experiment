(ns concurrency)

;;;;;
;; Demonstrates clojure concurrency capabilities
;;;;;

;; Atoms

;; !!! Use atoms when you need to update one data item at a time (atomics)

;; Defining an atom
(def a (atom 0))

;; Accessing an atom
(deref a)

;; Shortcut
@a

;; Compare And Swap
(swap! a inc) ;; 1
(swap! a + 2) ;; 3
(swap! a * 10) ;; 30

;; Warning! as swap! perform CAS operation it can try function application
;; several times. Just to note this function must be side-effect free

(swap! a (fn [a] (print a) a)) ;; prints a

;; Atomics validators
(def age (atom 0 :validator #(< % 200)))

(compare-and-set! age @age 150)
@age ;; 150

;; (compare-and-set! age @age 250)
;; Invalid Reference State Exception

;; Refs

;; !!! Use when you need to track a series of facts over time

;; Defining a ref
(def location (ref "Kiev"))
(def users (ref 20))

;; Accessing
(deref location)
@users

;; (ref-set location "Lviv")
;; No transaction running

;; Let's use a STM (transaction)
(dosync
 (ref-set location "Lviv")
 (alter users - 5))

;; Warning! Must be free of side-effects

;; Futures (like Java)

(def result (future (reduce *' (range 1 100000))))

;; Just delegating computation to another thread, not blocking current one

;; Agents

;; == Async atoms
;; Could contains side effects

(def printer (agent {}))

(defn print [msg]
  (println msg))

(defn print-async [printer msg]
  (send printer print msg))

;;(print-async printer "Hello, agent")

;; TODO detailed look at Agents


;; Promises

(def baby-name (promise))

;; @baby-name Blocks execution

(deliver baby-name "Oksana")

@baby-name ;; Oksana, woo hoo


;; What the difference between futures and promises?

;; Inverted responsibilities

;;  I want another thread to do my work for me and tell me the result: use a future.
;;  I want to do the work and tell the other thread when I’m done: use a promise.