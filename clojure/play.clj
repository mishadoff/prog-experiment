(ns detective.play
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; Simple engine
#_(run 1 [q]) ;; 0

;; Unify
#_(run 1 [q]
     (== 4 q)
     (== 5 q)) ;; ()

;; Local variables
#_(run 2 [q]
      (fresh [a b]
             (== a 4)
             (== b 2)
             (== q b))) ;; (2)
;; OR
#_(run* [q]
        (conde
         [(== q 2)]
         [(== q 1)]))

;; Membero
#_(run* [q]
      (membero q [1 2 3])
      (membero q [2 3 4])) ;; (2 3)

;; Conso
#_(run* [q]
      (conso 1 q [1 2 3]))

;; Resto
#_(run* [q]
        (resto [1 2 q 4] [2 3 4]))