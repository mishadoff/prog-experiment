(ns game2048)

(def SIZE 4)

(def world (->> (repeat SIZE 0)
                (vec)
                (repeat SIZE)
                (vec)))

(defn empty-cells [grid]
  (filter
   #(zero? (get-in grid %))
   (for [i (range SIZE) j (range SIZE)] [i j])))

(defn spawn [grid]
  (let [ec (empty-cells grid)]
    (if (< (count ec) 2) (throw (RuntimeException. "Game Over")))
    (reduce #(update-in %1 %2 + 2) grid (take 2 (shuffle ec)))))

(defn move [grid]
  "TODO")
