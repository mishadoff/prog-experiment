#lang racket
(provide (all-defined-out))

;; Lists test

;; sum of the numbers in a list
(define (sum xs)
  (if (null? xs) 0 (+ (car xs) (sum (cdr xs)))))

;; append
(define (my-append xs ys)
  (if (null? xs) ys
      (cons (car xs) (my-append (cdr xs) ys))))

;; map
(define (my-map f xs)
  (if (null? xs) null
      (cons (f (car xs))
            (my-map f (cdr xs)))))