(ns life-ui)

(def step (fn[g r](reduce(fn[i j](update-in i j(fn[v](get[v 1](-(apply +(map #(get-in g% 0)(for[a[-1 0 1]b[-1 0 1]](map + j[a b]))))v 2)0))))g r)))

;; 137 characters
;; Logic

(defn step-sugar [grid range]
  (let [m (count grid) 
        n (count (get grid 0))
        count-neighbours ;; determine number of neighbours
        (fn[[i j]]
          (reduce + (map #(get-in grid % 0)
                         (for[a [-1 0 1] b [-1 0 1]]
                           [(mod (+ i a) m) (mod (+ j b) n)]))))
        new-value  ;; calculate new value for cell
        (fn [v [i j]]
          (let [c (- (count-neighbours [i j]) v)]
            (cond (= 3 c) 1
                  (= 2 c) v
                  :else 0)))
        evolve-cell ;; update cell in a grid
        (fn[g i] (update-in g i #(new-value % i )))]
    (reduce evolve-cell grid range)))

;; Process

(defn evolution [g m n]
  (let [range (for [i (range m) j (range n)][i j])] ;; update whole grid
    (iterate #(step-sugar % range) g)))

(defn print-grid [g]
  (doseq [s (map #(apply str (replace {0 "." 1 "⚫"} %)) g)]
    (println s)))

;; Generator

(defn generate-grid [m n]
  (vec (for [i (range m)]
         (vec (for [j (range n)]
                (rand-int 2))))))

;; Samples

(def glider [[0 0 1 0 0 0 0 0]
             [1 0 1 0 0 0 0 0]
             [0 1 1 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]])

;; Main

(defn run [grid]
  (let [m (count grid) n (count (first grid))]
    (doseq [g (evolution grid m n)]
      (print-grid g)
      (Thread/sleep 1000))))

;; Swing

(import [java.awt Color Graphics Dimension]
        [javax.swing JPanel JFrame])

(def scale 20)
(def dimx 600)
(def dimy 600)
(def delay 250)

(defn draw-cell [^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn refresh-grid [graphics grid m n]
  (let [color (fn [n] (if (zero? n) Color/WHITE Color/BLACK))]
    (doseq [i (range m) j (range n)]
      (draw-cell graphics i j (color (get-in grid [i j]))))))

(defn run-swing [grid]
  (let [grid-ref (ref grid)
        m (count grid) n (count (first grid))
        panel (doto (proxy [JPanel] []
                      (paintComponent [g]
                        (proxy-super paintComponent g)
                        (refresh-grid g @grid-ref m n)))
                (.setPreferredSize (Dimension. dimx dimy)))
        frame (doto (JFrame.) (.add panel) .pack .show)]
    (doseq [gr (evolution grid m n)]
      (dosync (ref-set grid-ref gr))
      (.repaint panel)
      (Thread/sleep delay))))
