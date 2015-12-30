(ns newyear)

(defn tree [l r]
  (let [p print
        n println
        m(reduce +(take l(cons 3(iterate inc 2))))
        pc(fn[c n](dotimes[i n](p c)))
        pf(fn[f n](dotimes[i n](p(f))))
        ps #(pc" "%)
        d(fn[](if(<(rand)r)"o" " "))]
    (ps m)(n"★")
    (dotimes[cl l]
      (let [ld(nth(iterate inc 3)cl)
            bs(nth(reductions +(range))cl)
            b?(= cl(dec l))]
        (dotimes[h ld]
          (let [sb(- m h bs 1)sa(inc(* 2 h))a?(= h(dec ld))]
            (ps sb)(p"/")
            (cond
              (and a? b?)
              (pc"_"(+ sa(* 2 bs)))

              a?
              (do (p"_")
                  (pf d(+(- sa 2)(* 2 bs)))
                  (p"_"))

              :else
              (pf d(+ sa(* 2 bs))))
            (n "\\")))))
    (dotimes [i 2]
      (ps(- m 1))(p"|")
      (if (= i 1)(p"_")(p" "))
      (n"|"))))


(tree 5 0.2)


;; First version without decorations 1655
;; Decorations added 1712

;; Fixed 1535
;; First cut 859
