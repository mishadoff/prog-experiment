(ns brainfuck)

;; Brainfuck Interpreter in Clojure

;; Rules:
;; >         next cell
;; <         previous cell
;; +         increment value
;; -         decrement value
;; .         print value
;; ,         read value
;; [         start loop
;; ]         finish loop

(defn act [c cs p ]
  (case c
    \> (swap! p inc)
    \< (swap! p dec)
    \+ (aset cs @p (inc (nth cs @p))) 
    \- (aset cs @p (dec (nth cs @p)))
    \. (print (char (nth cs @p)))
    \, (aset cs @p (.read *in*))
))

(defn parse [s]
  (let [cs (int-array 100) p (atom 0)]
    (doseq [c s]
      (act c cs p))
    (print cs)))

(def parse-small

(fn[s](let[a(int-array 100)p(atom 0)u #(aset a@p%)e #(nth a@p)x #(swap! p%)](doseq[c s](case c\>(x inc)\<(x dec)\+(u(inc(e)))\-(u(dec(e)))\.(print(char(e)))\,(u(.read *in*))))))
                    ; ^
                    ; ^  
                    ; ^ 146 

;; 177 characters
;; -1(2) size of cells

)

