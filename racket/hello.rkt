#lang racket

(provide (all-defined-out))

(define x 3); val x = 3
(define y (+ x 2)) ;; + is a function

(define cube1
  (lambda (x)
    (* x (* x x))))

(define cube2
  (lambda (x)
    (* x x x)))

; syntactic sugar
(define (cube3 x)
  (* x x x))

(define s "Hello") ; string

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

; curried power function
(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

; currying
(define three-to-the (pow2 3))