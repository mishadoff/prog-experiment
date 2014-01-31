(ns patterns)

;;;;;;;;;;;;;;;;
;; 1. Command ;;
;;;;;;;;;;;;;;;;

(defn execute [command]
  (command))

;; What about parameters?

(defn execute [command & args]
  (apply command args))

;; What about history?

(def history (atom []))

(defn execute [command & args]
  (swap! history conj [command args]) ;; modifying state
  (apply command args))

;; (execute login "user" "password") 

;; Conclusion: Command is just a function

;;;;;;;;;;;;;;;;;
;; 2. Strategy ;;
;;;;;;;;;;;;;;;;;

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

;; Example sort

(def users [{:name "Lancelot" :subscription true}
            {:name "Arthur"   :subscription false}
             {:name "Percival" :subscription true}
            {:name "Galahad"  :subscription true}])

;; Stupid way
(sort (comparator 
       (fn [u1 u2]
         (cond
          (= (:subscription u1)
             (:subscription u2)) (neg? (compare (:name u1)
                                                (:name u2)))
             (:subscription u1) true
             :else false))) users)


;; Oneliners
(sort-by (juxt (complement :subscription) :name) users)
(sort-by (juxt :subscription :name) #(compare %2 %1) users)


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

;; Custom datastructure?

(deftype ReverseArray [vec]
  clojure.lang.Seqable
  (seq [self] (seq (reverse vec))))

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

(def mr-white   (atom {:name "Mr. White"   :state :enabled}))
(def mr-pink    (atom {:name "Mr. Pink"    :state :staring}))
(def mr-blonde  (atom {:name "Mr. Blonde"  :state :subscription}))

(defmulti switch :state)

(defmethod switch :enabled [user]
  (println (:name user) "is disabled.")
  (assoc user :state :disabled))

(defmethod switch :disabled [user]
  (println (:name user) "is enabled.")
  (assoc user :state :enabled))

(defmethod switch :staring [user]
  (println (:name user) "is staring. Disable.")
  (assoc user :state :disabled))

(defmethod switch :subscription [user]
  (println (:name user) "has subscription.")
  user)

@mr-blonde
(swap! mr-blonde switch)

;;;;;;;;;;;;;;;;;;;;;;;;
;; 9. Template Method ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Move character in RPG world

;; (defn move-to [loc]
;;   (cond
;;    (chest? loc) (let [chest (:chest loc)]
;;                   (if (open? chest) 
;;                     (take-from chest)
;;                     (do-nothing)))
;;    (enemy? loc) (attack :melee (:enemy loc))
;;    (quest? loc) (accept quest)
;;    (artifact? loc) (let [art (:art loc)]
;;                      (if (better :art (my))
;;                        (replace :art)))))
  
;; Does it differ from strategy? Answer: NO.

;;;;;;;;;;;;;;;;;;
;; 10. Visitor ;;;
;;;;;;;;;;;;;;;;;;

(derive ::fastfood ::food)

(defmulti eat (fn [a b] [a b]))

(defmethod eat [::coder ::food] [x y]
  (println "Coder eats Food"))
  
(defmethod eat [::coder ::fastfood] [x y]
  (println "Coder eats FastFood"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 11. Chain of responsibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def files ["hello.txt" "file.json"])

(defn process-txt [file]
  (println "Processing TXT: " file) true)
  
(defn process-json [file]
  (println "Processing JSON: " file) true)

(defn process-xml [file]
  (println "Processing XML: " file) true)

(defn process-edn [file]
  (println "Processing EDN: " file) false)


;; old-school way ;; SNIPPETS
(defn chain [[f & fs] & args]
  (when f
    (apply f args)
    (recur fs args)))


;; Cut-off
(defn chain [[f & fs] & args]
  (when f
    (let [result (apply f args)]
      (if result (recur fs args) nil))))




;; CREATIONAL

;;;;;;;;;;;;;;;;;;
;; 1. Prototype ;;
;;;;;;;;;;;;;;;;;;

;; Clone needed for preventing mutability.
;; Clojure immutable.

;; Assume you must create a set of objects sharing the same structure

(def student-prototype
  {:name "TODO"
   :age 20
   :university "KPI"
   :faculty "AMD"})

;; You can create new student by just modifing this one, old ones not gone

(assoc student-prototype :name "Johnny Depp")

;; Create a lot object from names
(def names ["Brad" "Angelina" "Rudy"])
(def students (mapv #(assoc student-prototype :name %) names))

;;;;;;;;;;;;;;;;;;
;;; 2 Singleton ;;
;;;;;;;;;;;;;;;;;;

;; Singleton -> Evolution

;; public final class Singleton...
;; public enum Singleton { INSTANCE; }

;; Believe or not it is just a fancy global variable
(def singleton [:data])

;; All concurrency problems came in if you want to make it lazy

(defn make-runtime []
  {:processors 4
   :memory 4096})

;;;;;;;;;;;;;;;;;;
;; 3. Builder ;;;;
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

;; Incosisten state validation with {:pre}

;; Named parameters, lack in java
;; new Object(name: a, city: b)

;; In clojure

;; (defn make-object [& {:keys [name city]}])
;; 
;; To call
;; (make-object :name "Misha" :city "Kiev")

;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Factory Method  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates an object

;; Why constructors are bad?
;; They don't have clear name

;; Bloch arguments?

;; Complex number can have multiple representations:
;;
;; (defrecord Complex [re im])
;; {:re 0 :im 0}
;; [0 0]

;; (->Complex 1 2)


;;; END

;; NICCY is abbreviation of CYNIC
;; VAINE is abbreviation of NAIVE
