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

(declare loop-)

(defn act [a cs p i]
  (case (nth cs @i)
    \> (swap! p inc)
    \< (swap! p dec)
    \+ (aset a @p (inc (nth a @p))) 
    \- (aset a @p (dec (nth a @p)))
    \. (print (char (nth a @p)))
    \, (aset a @p (.read *in*))
    \[ (if (zero? (nth a @p)) (loop- cs i inc dec))
    \] (if-not (zero? (nth a @p)) (loop- cs i dec inc))
))

(defn loop- [cs i f g]
  (loop [level 1]
   (when-not (zero? level)
     (swap! i f)
     (case (nth cs @i)
       \] (recur (g level))
       \[ (recur (f level))
       (recur level)))))

(defn parse [s]
  (let [a (int-array 100) p (atom 0)
        cs (vec (seq s)) n (count cs)]
    (loop [i (atom 0)]
      (act a cs p i)
      (swap! i inc) ;;
      (if-not (= n @i) (recur i)))))
           
           
(def parse-small

(fn[s](let[a(int-array 100)p(atom 0)c(vec(seq s))n(count c)
l(fn[i f g](loop[level 1](when-not(zero? level)(swap! i f)(case(nth s@i)
\](recur(g level))\[ (recur (f level))(recur level)))))]
(loop[i(atom 0)](case (nth c @i)\>(swap! p inc)\< (swap! p dec)
\+(aset a@p(inc(nth a@p)))\-(aset a@p(dec(nth a@p)))\.(print(char(nth a@p)))
\,(aset a@p(.read *in*))\[(if(zero?(nth a@p))(l i inc dec))
\](if-not(zero?(nth a@p))(l i dec inc)))(swap! i inc)(if-not(= n @i)(recur i)))))

;; 472 characters

)

