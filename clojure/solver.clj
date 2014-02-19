(ns solver)

;; Solves 3 4 5 6  =  28


(def ops [+ - * /])


(defn merge [nums]
  (if (empty? nums) 0
      (bigint (apply str nums))))
  
(defn solve-op [a b]
  (let [res [(+ a b)
             (- a b)
             (* a b)]]
    (if (zero? b) res (conj res (/ a b)))))

(defn solve-all [a b]
  (concat 
   (solve-op a b)
   (solve-op (- a) b)
   (solve-op a (- b))
   (solve-op (- a) (- b))))

(defn solve [es]
  (if (> (count es) 1)
    (apply concat 
     (for [i (range 1 (inc (count es)))]
       (mapcat (fn [res] 
                 (solve-all (merge (take i es)) res)) 
               (solve (drop i es)))))
    [(merge es)]))
