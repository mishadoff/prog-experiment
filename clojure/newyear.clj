(ns newyear)

(defn tree [level ratio]
  (let [mid-shift (nth (reductions + (cons 3 (iterate inc 2))) (dec level))
        decor (fn []
                (print 
                 (if (< (rand) ratio) "o" " ")))
        ] ;; calculating properties
    ;; print star
    (dotimes [i mid-shift] (print " "))
    (print "★")
    (println)

    (dotimes [current-level level]
      (let [level-depth (nth (iterate inc 3) current-level)
            block-shift (nth (reductions + (range)) current-level)
            last-level? (= current-level (dec level))]
        ;; print level
        (dotimes [block0 level-depth]
          (let [spaces-before (- mid-shift block0 block-shift 1)
                spaces-after (inc (* 2 block0))
                last? (= block0 (dec level-depth))]
            (dotimes [i spaces-before] (print " "))
            (print "/")
            (cond
              (and last-level? last?)
              (dotimes [i (+ spaces-after
                             (* 2 block-shift))] (print "_"))
              
              last?
              (do
                (print "_")
                (dotimes [i (+ (- spaces-after 2)
                               (* 2 block-shift))] (decor))
                (print "_"))

              :else
              (do
                (dotimes [i (+ spaces-after
                               (* 2 block-shift))] (decor)))
              )
            
            (print "\\")
            (println)
            )
          )
        

        )

      )

    ;; print stem
    (dotimes [i 2]
      (dotimes [i (- mid-shift 1)] (print " "))
      (print "|")
      (if (= i (dec 2))
        (print "_")
        (print " "))
      (println "|")
      
      )
    
    ))


(tree 5 0.2)


;; First version without decorations 1655
;; Decorations added 1712
