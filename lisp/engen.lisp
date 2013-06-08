;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; engen - English sentences generator
;; PAIP / Chapter II 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; English Grammar Subset
;; S -> NP VP
;; NP -> A N
;; VP -> V NP
;; A -> the, a, ...
;; N -> man, woman, ...
;; V -> hit, took, ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First version

(defun one-of (set)
  "Pick random element from the set"
  (list (elt set (random (length set)))))

(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (article) (noun)))
(defun verb-phrase () (append (verb) (noun-phrase)))
(defun article () (one-of '(THE A)))
(defun noun () (one-of '(MAN WOMAN)))
(defun verb () (one-of '(HIT TOOK LOVE)))

;; generator
(sentence)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; English Grammar Subset Extended
;; S -> NP VP
;; NP -> A ADJ* N PP*
;; VP -> V NP
;; PP -> PR NP
;; ADJ -> big, little, blue, green
;; PR -> to, in, by, with
;; A -> the, a, ...
;; N -> man, woman, ...
;; V -> hit, took, ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (article) (adj*) (noun) (pp*)))
(defun pp () (append (prep) (noun-phrase)))
(defun adj () (one-of '(BIG LITTLE BLUE GREEN)))
(defun prep () (one-of '(TO IN BY WITH ON)))
(defun verb-phrase () (append (verb) (noun-phrase)))
(defun article () (one-of '(THE A)))
(defun noun () (one-of '(MAN WOMAN)))
(defun verb () (one-of '(HIT TOOK LOVE)))

(defun adj* ()
  (if (= (random 2) 0) nil
    (append (adj) (adj*))))
(defun pp* ()
  (if (= (random 2) 0) nil
    (append (pp) (pp*))))

(sentence)
