(ns bullseye)

;; 2n^2 - 2n + 2rn

(defn solve [r t]
  "r - initial radius, t - mls of paint"
  (letfn [(nrings [n]
            (+' (*' 2 n n)
                (-' n)
                (*' 2 r n)))
          (binary [[a b]]
            (cond (or (= a b) (= (inc a) b)) a
                  :else (let [avg (quot (+' a b) 2)
                              pnt (nrings avg)]
                          (if (<= pnt t)
                            (recur [avg b])
                            (recur [a avg])))))]
    (binary [0 1000000000000000000])))

(defn solve-file [file]
  (->> (slurp file)
       (re-seq #"\d+")
       (map bigint)
       (rest)
       (partition 2 2)
       (map-indexed #(vec [(inc %1) (apply solve %2)]))
       (map #(format "Case #%d: %d" (first %) (last %)))
       (interpose "\n")
       (apply str)
       (spit "D:\\bulleye.txt")))
