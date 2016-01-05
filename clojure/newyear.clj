(ns newyear)

(def tree
  (fn[l r]
    (let[p print
         n println
         e range
         m(reduce +(take l(cons 3(iterate inc 2))))
         f(fn[w n](dotimes[i n](p(if(fn? w)(w)w))))
         d #(f(fn[](if(<(rand)r)\o\ ))%)]
      (f" "m)(n\★)
      (doseq[b(e l)h(e(+ b 3)):let[u(quot(*(+ 1 b)b)2)v(+(* 2 h)1)a(=(+ b 2)h)t(+ v u u)]](f\ (- m h u 1))(p\/)
            (cond
              (and(=(dec l)b)a)(f\_ t)
              a(do(p\_)(d(- t 2))(p\_))
              1(d t))
            (n\\))
      (f\ (- m 1))(n"| |")
      (f\ (- m 1))(n"|_|"))))


(tree 4 0.2)


(defmacro dotimes-x [bindings & body]
  )

;; First version without decorations 1655
;; Decorations added 1712

;; Fixed 1535
;; First cut 859
;; 819
;; 760
;; 593
;; 574
;; 555
;; 546
;; 534
;; 532 NO WS TRUNCATION
