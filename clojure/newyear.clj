(ns newyear)

(defn tree [level ratio]
  (let [mid (reduce + (take level (cons 3 (iterate inc 2))))
        print-char-n (fn [c n] (dotimes [i n] (print c)))
        print-fn-n (fn [f n] (dotimes [i n] (print (f))))
        print-spaces (partial print-char-n " ")
        decoration (fn [] (if (< (rand) ratio) "o" " "))]
    ;; print star

    (print-spaces mid)
    (println "★")

    (dotimes [current-level level]
      (let [level-depth (nth (iterate inc 3) current-level)
            block-shift (nth (reductions + (range)) current-level)
            last-level? (= current-level (dec level))]
        ;; print level
        (dotimes [line level-depth]
          (let [spaces-before (- mid line block-shift 1)
                spaces-after (inc (* 2 line))
                last? (= line (dec level-depth))]
            (print-spaces spaces-before)
            (print "/")
            (cond
              (and last-level? last?)
              (print-char-n "_" (+ spaces-after (* 2 block-shift)))

              last?
              (do
                (print "_")
                (print-fn-n decoration (+ (- spaces-after 2) (* 2 block-shift)))
                (print "_"))

              :else
              (print-fn-n decoration (+ spaces-after (* 2 block-shift)))
              
              )
            
            (println "\\")))))
    
    ;; print stem
    (dotimes [i 2]
      (print-spaces (- mid 1))
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

;; Fixed 1535
