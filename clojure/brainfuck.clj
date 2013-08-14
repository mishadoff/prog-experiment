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


;; Loop can be rewritten to remove nested
(defn loop- [cs i f g]
  (loop [level 1]
   (when-not (zero? level)
     (swap! i f)
     (case (nth cs @i)
       \] (recur (g level))
       \[ (recur (f level))
       (recur level)))))

(defn loop-new- [cs i f g]
  (loop [level 1]
   (when-not (zero? level)
     (swap! i f)
     (case (nth cs @i)
       \] (recur (g level))
       \[ (recur (f level))
       (recur level)))))

(defn loop-a [cs i f]
  (loop []
    (do (swap! i f))
    (if-not (or (= \[ (nth cs @i)) (= \] (nth cs @i))) (recur))))

;; if [ find next ]
;; if ] find next [
(defn loop-num [s]
  )


(defn parse [s]
  (let [a (int-array 100) p (atom 0)
        cs (vec (seq s)) n (count cs) i (atom 0)]
    (while (not= n @i)
      (do
        (act a cs p i)
        (swap! i inc)))))
           
+1
           
(def parse-internal

(fn[a p k c](let[h #(nth %1@%2)e #(h a p)s #(swap! %1%2 1)t #(aset a@p(%(e)1))l #(do(s k %)(case(h c k)\]()\[()(recur %)))](while(>(count c)@k)(do(case(h c k)\>(s p +)\<(s p -)\+(t +)\-(t -)\.(print(char(e)))\,(aset a@p(.read *in*))\[(if(=(e)0)(l +))\](if(>(e)0)(l -)))(s k +)))))

;; 275

;; if as map function
;; (.read *in*) =>  [1]
;; (int(read)) =>     
;; inc dec + -

)

(defn parse-internal* [a pt pc cs]
  (letfn [(act []
            (case (nth cs @pc)
              \> (swap! pt inc)
              \< (swap! pt dec)
              \+ (aset a @pt (inc (nth a @pt))) 
              \- (aset a @pt (dec (nth a @pt)))
              \. (print (char (nth a @pt)))
              \, (aset a @pt (.read *in*))
              \[ (if (zero? (nth a @pt)) (loop- inc))
              \] (if-not (zero? (nth a @pt)) (loop- dec))))
          (loop- [f]
            (do (swap! pc f)
                (case (nth cs @pc)
                  \[ ()
                  \] () 
                  (recur f))))]
    (while (not= (count cs) @pc)
      (do
        (act)
        (swap! pc inc)))))


(defn parse-small [s f]
  (let [a (int-array 100)  ;; Turing Tape
        p (atom 0)         ;; Pointer to Tape
        k (atom 0)         ;; Pointer to Command
        c (vec (seq s))]    ;; Vector of commands
    (f a p k c)))


(fn[f](print 1 f)(case(+ 1 2)\] 0 \[ 0(recur f))) ;(if-not(or(=\[(+ 1 2))(=\](- 2 4)))(recur f)))

;(fn[f](s k f)(case(n c k)\]()\[()(recur f)))
;#(do(+ 1 2)(case(- 1 2)\]()\[()(recur %)))

;(if(not=(e)0)(l d))
;(({0#()}(e)#(l d)))

;;(if(= 1 0)1)
;(({0 #(print 1)}1#()))

(def hello-world "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")

(def reverse-print3 ",>,>,.<.<.")

(def reverse-10 "+++++[>,.<-]")


