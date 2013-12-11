(ns patterns)

;; Scratch for article/prez

;; 1. Old School Design Patterns (GoF) in Clojure

;; 2. Why Patterns?
;; Our programing language is fucked up. 
;; That's why we need design patterns

;; 3. HARD DISCLAIMER
;; - Most actions are simplified because of DYNAMIC PROGRAMMING
;; - You may be not agree, it's ok.

;; 4. Three Tankists and Dog
;; Structural, Creational, Behavioural and Java

;;;;;;;;;;;;;;;;
;; 1. Command ;;
;;;;;;;;;;;;;;;;

;; "Encapsulates information needed to call a method at a later time" WAT?

;; Example: java.lang.Runnable

;; Maybe functional interface?
;; Maybe functional int...
;; Maybe function...

;; Real World Example: TODO 

;; Java -> Interface Command.action()
;; Clojure -> function

;; Just Wrap In Anonymous noarg function if you need delayed computation
;; By the way functional programmers call it thunk (delayed computation)

(defn execute-command [command]
  (command))

;; JAVA:   What about history?

;; History it's a state.

(def history (atom []))

(defn execute-command-with-history [command]
  (swap! history conj command) ;; modifying state
  (command))

;; CLOJURE:   What about parameters?

;; can pass any number of parameters
(defn execute-command-with-args [command & args]
  (apply command args))

;; or caller can use no-arg anonymous function (execute #(switch :on))

;; Conclusion: Command is just a function

;;;;;;;;;;;;;;;;;
;; 2. Strategy ;;
;;;;;;;;;;;;;;;;;

;; "Algorithm's behaviour can be selected at runtime" WAT?

;; Example: Collections.sort(coll, comparator) in Java
;                     Algorithm     behaviour

(defn execute [data behaviour]
  (apply behaviour data))

(defn sorted? [coll comp]
  (apply comp coll))

(sorted? [1 2 3] <)
(sorted? [5 2 2 1] >)
(sorted? [5 2 2 1] >=)

(defn sorted? [coll context]
  (let [{:keys [fun mess]} context]
    (print mess)
    (apply fun coll)))

(sorted? [1 2 3] {:fun < :mess "Less"})

;; Conclusion: Strategy is just function accepts function

;;;;;;;;;;;;;;;;;
;; 3. Iterator ;;
;;;;;;;;;;;;;;;;;

;; "Iterator is an object that enables a programmer to traverse a container" WAT?

;; Object -> Function

;; Example: java.util.Iterator

;; Iterator interface defines a set of functions...
;; hasNext(), next()

;; Iterator iterator;
;; while (iterator.hasNext()) {
;;   iterator.next();
;; }

;; Guess what? It's a singly-linked list interface. 

;; Iterator = Sequence
(seq [1 2 3]) ;; Vector
(seq (list 1 2 3)) ;; List
(seq #{1 2 3}) ;; Set
(seq (int-array 5 1)) ;; Array

;; It returns a LIST. List naturally have operations hasNext() and next()

(first [1 2 3]) ;; => 1 
(rest [1 2 3]) ;; => (2 3)

;; Conclusion: Iterator is abstraction and two functions.
;; Overabstraction of single linked list?

;;;;;;;;;;;;;;;;;;;;
;; 4. Interpreter ;;
;;;;;;;;;;;;;;;;;;;;

;; "Interpreter pattern is a design pattern that specifies how to evaluate sentences in a language"

;; Example: java.util.Pattern, java.text.Format

;; Set of matches. 
;; Basically it's close to pattern matching, 
;; but clojure have a lack o pm. Java either.

;; Guys, it's LISP. Do you really want to fight?
;; Real-world Example: Reverse Polish Notation


;; TODO show them magic
;; Macro support

;; Conclusion: HAHAHAHAHA!!

;;;;;;;;;;;;;;;;;
;; 5. Mediator ;;
;;;;;;;;;;;;;;;;;

;; "Define an object that encapsulates how a set of objects interact"

;; Example: java.lang.reflect.Method#invoke()

(defn mediator-action [obj1 obj2]
  (do obj1)
  (do obj2))

;;;;;;;;;;;;;;;;
;; 6. Memento ;;
;;;;;;;;;;;;;;;;

;; "Provides the ability to restore an object to its previous state"

;; Undo functionality?

;; Example: java.io.Serializable

;; Again based on state. In clojure we don't have stated, instead.

(def states (atom []))

(defn save-state [state]
  (swap! states conj state))

(defn restore-to-last []
  (let [restored (peek @states)] 
    (swap! states pop)
    restored))

;; Usage

(save-state :open)
(save-state :run)
(save-state :active)

(restore-to-last)

;;;;;;;;;;;;;;;;;
;; 7. Observer ;;
;;;;;;;;;;;;;;;;;

;; Actually, harder a bit

;;;;;;;;;;;;;;
;; 8. State ;;
;;;;;;;;;;;;;;

;; O'no state

(defmulti switch :state)

(defn flip [context]
  (->> (:state context)
       (get {:on :off :off :on})
       (assoc context :state)))           

(defmethod switch :on [context]
  (println (:obj context) "is ON")
  (flip context))
 
(defmethod switch :off [context]
  (println (:obj context) "is OFF")
  (flip context))


;; Another approach

(def context (atom {:state :on :obj "TV"}))

(defn switch []
  (let [{:keys [state obj]} @context]
    (println obj "is" state) 
    (swap! context flip)))

(repeatedly 10 switch)

;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Template Method ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn abstract-algorithm [f]
  (println "step 1")
  (println "step 2")
  (f)
  (println "step 3"))

;; Does it differ from strategy? Answer: NO.

;;;;;;;;;;;;;;;;;;
;; 10. Visitor ;;;
;;;;;;;;;;;;;;;;;;

;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Chain of responsibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO simple




;; CREATIONAL


;;;;;;;;;;;;;;;;;;
;; 1. Prototype ;;
;;;;;;;;;;;;;;;;;;

;; Clone needed for preventing mutability.
;; Clojure immutable.

;;;;;;;;;;;;;;;;;;
;;; 2 Singleton ;;
;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;
;; 3. Composite ;;
;;;;;;;;;;;;;;;;;;

;; Just a Tree structure

;;;;;;;;;;;;;;;;;;
;; 4. Builder ;;;;
;;;;;;;;;;;;;;;;;;

;; What is builder?
;; It creates object.

;; Why constructor is bad?
;; Object(a, b)

;; Optional parameters!
;; Object(a, b, c)
;; Object(a, b)
;; Object(a, c)

;; In case of 3 optional parameters we have 8 different constructors
;; Telescope hell

;; what about setters?
;; It can cause inconsistent state for object

;; Named parameters, lack in java
;; new Object(name: a, city: b)

;; In clojure

;; (defn make-object [& {:keys [name city]}])
;; 
;; To call
;; (make-object :name "Misha" :city "Kiev")

;; StringBuilder in clojure?
;; (def sb (atom []))
;;
;; (defn sb-append [s]
;;   (swap! sb conj s))
;;
;; (defn build [sb]
;;   (apply str @sb)) ;; creating a string


;;;;;;;;;;;;;;;;;;
;; 5. Factory ;;;;
;;;;;;;;;;;;;;;;;;
