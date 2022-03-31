#lang racket

; Exericse 1.4 in Structure and Interpretation of Computer Programs
(define (a-plus-abs-b a b) 
    ((if (> b 0) + -) a b))

; (a-plus-abs-b 10 -5)


; Exercise 1.5 in Structure and Interpretation of Computer Programs
(define (p) (p))

(define (test x y)
    (if (= x 0) 0 y))

; (test 0 (p))
