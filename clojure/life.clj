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
          (vec(for[j n v (x g[i j]) c (-(apply +(map #(x g% 0)(for[a[-1 0 1]b[-1 0 1]][(+ a i)(+ b j)])))v)]
                (if(= 3 c)1(if(= 2 c) v 0))))))))

;; Trying to remove spaces
(fn[s g](let[x get-in n(range s)](vec(for[i n](vec(for[j n :let[[[a b][c d]](map(juxt dec inc)[i j])c(apply +(map #(x g% 0)[[a c][a j][a d][i c][i d][b c][b j][b d]]))]](if(= 3 c)1(if(= 2 c)(x g[i j])0))))))))

;; 209
