(ns awesome)

;; List of awesome clojure snippets

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Sort list of maps by multi values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO swap first name with lastname
;; Use Case:
;; Sort list of people by firstname, and if
;; first names are equal, sort them by lastname

;; Data example
(def data [{:firstname "Alan" :lastname "Perlis"}
           {:firstname "John" :lastname "Kail"}
           {:firstname "John" :lastname "Doe"}
           {:firstname "John" :lastname "White"}
           {:firstname "Walt" :lastname "White"}])

;; Wrong
(sort (comparator
       (fn [p1 p2]
         (let [r (compare (:firstname p1) (:firstname p2))]
           (if (zero? r) (neg? (compare (:lastname p1) (:lastname p2))) (neg? r)))))
      data)

;; 1.1 Unnecessary comparator wraping
;; 1.2 code repetition

;; Correct
(sort-by (juxt :firstname :lastname) data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Compare results of two applied functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test commit
;; TODO real use case

;; TODO functions specific to use case
(defn some-func-1 [o1]
  ;; Some work here
  o1)

(defn some-func-2 [o1]
  ;; Some work here
  o1)

(defn equal? [o1 o2]
  (= (some-func-2 (some-func-1 o1))
     (some-func-2 (some-func-1 o2))))

;; Use function composition, it is better to understand code

(defn equal2? [o1 o2]
  (= ((comp some-func-1 some-func-2) o1)
     ((comp some-func-1 some-func-2) o2)))

;; Avoid code repetition

(defn equal3? [o1 o2]
  (let [f (comp some-func-1 some-func-2)]
    (= (f o1) (f o2))))
