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
    \+ (assoc! cs @p (inc (nth cs @p))) 
    \- (assoc! cs @p (dec (nth cs @p)))
    \. (print (char (nth cs @p)))
    \, (assoc! cs @p (.read *in*))
))

(defn parse [s]
  (let [cs (transient (vec (repeat 100 0))) p (atom 0)]
    (doseq [c s]
      (act c cs p))
    (print (persistent! cs))))
