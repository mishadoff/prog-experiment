(ns life)

;; do not count
(def grid [[0 0 0]
           [1 1 1]
           [0 0 0]])

(defn count-of-neighbours [grid i j]
  (let [i- (dec i) i+ (inc i)
        j- (dec j) j+ (inc j)]
    (reduce + (map #(get-in grid % 0)
                   [[i- j-] [i- j] [i- j+] [i j-] [i j+] [i+ j-] [i+ j] [i+ j+]]))))

(defn step [grid]
  (let [n (count grid)]
    (vec (for [i (range n)]
      (vec (for [j (range n)]
        (let [c (count-of-neighbours grid i j)
              v (get-in grid [i j])]
          (cond (= 3 c) 1
                (= 2 c) v
                :else 0))))))))

;; do not count
(defn run [n grid]
  (take n (iterate step grid)))

;; First version 489 characters...

; 1. count-of-neighbours used just once, but have such a long name
; 2. i-, i+, dec, inc seems redundant

(defn step2 [grid]
  (let [n (count grid)]
    (vec (for [i (range n)]
      (vec (for [j (range n)]
        (let [c
              (let [[[a b] [c d]] (map (juxt dec inc) [i j])]
                (reduce + (map #(get-in grid % 0)
                               [[a c] [a j] [a d] [i c] [i d] [b c] [b j] [b d]])))
              v (get-in grid [i j])]
          (cond (= 3 c) 1
                (= 2 c) v
                :else 0))))))))

;; 433 chars

; 2. Rename variable grid, range to let, reduce -> apply
(defn step4[s g]
  (let[x get-in n(range s)]
    (vec(for[i n]
          (vec(for[j n :let[[[a b][c d]](map(juxt dec inc)[i j])
                            c(apply +(map #(x g% 0)[[a c][a j][a d][i c][i d][b c][b j][b d]]))]]
                (if(= 3 c)1(if(= 2 c)(x g[i j])0))))))))

(defn step5[s g]
  (let[x get-in n(range s)]
    (vec(for[i n]
          (vec(for[j n]
                (let[t[-1 0 1]v(x g[i j])c(-(apply +(map #(x g% 0)(for[a t b t][(+ i a)(+ j b)])))v 2)]
                  (x[v 1][c]0))))))))

(defn forr [n]
  (vec (for [i (range n)] i)))

;; 165 with spaces and anonymous function

;; CHEAT with changing api to range
;; PASS get-in function
;; PASS [-1 0 1] array
(defn step6[r x t g]
  (vec(for[i r]
        (vec(for[j r]
              (let[v(x g[i j])c(-(apply +(map #(x g% 0)(for[a t b t][(+ i a)(+ j b)])))v 2)]
                (x[v 1][c]0)))))))

;; 134

(defn step5[s g]
  (let[x get-in n(range s)]
    (vec(for[i n]
          (vec(for[j n]
                (let[t[-1 0 1]v(x g[i j])c(-(apply +(map #(x g% 0)(for[a t b t][(+ i a)(+ j b)])))v 2)]
                  (x[v 1][c]0))))))))

;; passing ranges
(defn step6[q g]
  (let[x get-in z(fn[r](let[t[-1 0 1]v(x g r)c(-(apply +(map #(x g% 0)(for[a t b t](map + r[a b]))))v 2)](x[v 1][c]0)))]
    (reduce #(assoc-in%%2(z%2))g q)))

;; 160

(def step7 (fn[g r](reduce(fn[i j](update-in i j(fn[v](get[v 1](-(apply +(map #(get-in g% 0)(for[a[-1 0 1]b[-1 0 1]](map + j[a b]))))v 2)0))))g r)))

;; 137
;; Achievment unlocked!

;; readability

(defn step-sugar [grid range]
  (let [count-neighbours ;; determine number of neighbours
        (fn[[i j]]
          (reduce + (map #(get-in grid % 0)
                         (for[a [-1 0 1] b [-1 0 1]]
                           [(+ i a) (+ j b)]))))
        new-value  ;; calculate new value for cell
        (fn [v [i j]]
          (let [c (- (count-neighbours [i j]) v)]
            (cond (= 3 c) 1
                  (= 2 c) v
                  :else 0)))
        evolve-cell ;; update cell in a grid
        (fn[g i] (update-in g i #(new-value % i )))]
    (reduce evolve-cell grid range)))

;; Usage

(defn evolution [g]
  (let [p (count g) q (count (get g 0)) ;; dimensions of grid
        range (for [i (range p) j (range q)][i j])] ;; update whole grid
    (iterate #(step7 % range) g)))

(defn print-grid [g]
  (doseq [s (map #(apply str (replace {0 "." 1 "⚫"} %)) g)]
    (println s)))

;; TESTING

(defn run [grid]
  (doseq [g (evolution grid)]
    (print-grid g)
    (Thread/sleep 1000)))

(def block [[0 0 0 0]
            [0 1 1 0]
            [0 1 1 0]
            [0 0 0 0]])

(def blinker [[0 0 0]
              [1 1 1]
              [0 0 0]])

(def toad [[0 0 0 0 0 0]
           [0 0 0 0 0 0]
           [0 0 1 1 1 0]
           [0 1 1 1 0 0]
           [0 0 0 0 0 0]
           [0 0 0 0 0 0]])

(def beacon [[1 1 0 0]
             [1 1 0 0]
             [0 0 1 1]
             [0 0 1 1]])

(def glider [[0 0 1 0 0 0 0 0]
             [1 0 1 0 0 0 0 0]
             [0 1 1 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 0 0]])