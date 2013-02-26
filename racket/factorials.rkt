#lang racket
(provide (all-defined-out))

; simple
(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))

; tco
