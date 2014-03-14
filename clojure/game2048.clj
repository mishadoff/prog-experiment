(ns game2048)

(def SIZE 4)

(def world (->> (repeat SIZE 0)
                (vec)
                (repeat SIZE)
                (vec)))

(defn- empty-cells [grid]
  (filter
   #(zero? (get-in grid %))
   (for [i (range SIZE) j (range SIZE)] [i j])))

(defn- spawn [grid]
  (let [ec (empty-cells grid)]
    (if (< (count ec) 2) (throw (RuntimeException. "Game Over")))
    (reduce #(update-in %1 %2 + 2) grid (take 2 (shuffle ec)))))

(defn- fold [line]
  (->> (remove zero? line)
       (partition-by identity)
       (map (fn [[p & _ :as part]]
              (condp = (count part)
                1 p 
                2 (* 2 p)
                3 [(* 2 p) p]
                4 [(* 2 p) (* 2 p)])))
       (flatten)
       (#(concat % (repeat (- 4 (count %)) 0)))
       (vec)))

(defn- move-left [grid]
  (mapv fold grid))

(defn- move-down [grid]
  )

;; Visualisation

(defn- printg [grid]
  (doseq [line grid]
    (println line)))
