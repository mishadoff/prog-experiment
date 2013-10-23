(ns patterns)

;; Scratch for article

;; Old School Design Patterns in Clojure
;; Our programing language is fucked up. So we need design pattern

;;;;;;;;;;;;;;;;
;; 1. Command ;;
;;;;;;;;;;;;;;;;

;; "Encapsulates information needed to call a method at a later time" WAT?

;; Examples:
;; java.lang.Runnable
;; javax.swing.Action
;; com.google.gwt.user.client.Command

;; Maybe functional interface?
;; Maybe functional int...
;; Maybe function...

(defn create-command [f & args]
  #(apply f args)) 

;; Just Wrap In Anonymous noarg function if you need delayed computation
;; By the way functional programmers call it thunk (delayed computation)

(defn execute-command [command]
  (command))

;; Java Attacks: What about history?

(def history (atom []))

(defn execute-command-history [command]
  (swap! history conj command)
  (command))

;; Clojure Attacks: Please, compose commands.

;; http://stackoverflow.com/questions/9847245/pattern-for-implementing-undo-redo-in-clojure

;; Conclusion: Command is just a function

;;;;;;;;;;;;;;;;;
;; 2. Strategy ;;
;;;;;;;;;;;;;;;;;

;; "Algorithm's behaviour can be selected at runtime" WAT?

;; Example Collections.sort(coll, comparator) in Java

(defn action [coll f]
  (reduce f coll))

;; Conclusion: Strategy is just function that accepts function

;;;;;;;;;;;;;;;;;
;; 3. Iterator ;;
;;;;;;;;;;;;;;;;;

(seq [1 2 3])

;; Conclusion: Iterator is just a good abstraction. Clojure has it

;;;;;;;;;;;;;;;;;;;;
;; 4. Interpreter ;;
;;;;;;;;;;;;;;;;;;;;

;; Guys, it's LISP. Do you really want to fight?

;; TODO show them magic
;; Macro support

;; Conclusion: HAHAHAHAHA!!

;;;;;;;;;;;;;;;;;
;; 5. Mediator ;;
;;;;;;;;;;;;;;;;;

;; To communicate between objects. We don't really have objects

;; TODO

;;;;;;;;;;;;;;;;
;; 6. Memento ;;
;;;;;;;;;;;;;;;;

;; State??? You're doing it wrong. Instead we just replace

(def states (atom []))

(defn save-state [state]
  (swap! states conj state))

;; restore


;;;;;;;;;;;;;;;;;
;; 7. Observer ;;
;;;;;;;;;;;;;;;;;

;; Actually, harder a bit

;;;;;;;;;;;;;;
;; 8. State ;;
;;;;;;;;;;;;;;

;; O'no state

(defmulti print-string :state)

(defmethod print-string :upper [s]
  (println (.toUpperCase (:message s))))

(defmethod print-string :lower [s]
  (println (.toLowerCase (:message s))))

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

;; Clone needed for state. In clojure we don't use state.
