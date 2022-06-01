#lang gamble
(require gamble/viz)

; exercise 6
; geometric distribution defined by stochastic recursion
(define (geometric p)
  (if (flip p)
      0
      (+ 1 (geometric p))))

; number of samples we want to generate
(define n-samples 100000)

; model used to generate the samples
(define (geometric-model)
  (define p 0.5)
  (geometric p))

; procedure which counts the number of samples with value 5
(define (count-5 l)
  (if (null? l)
      0
      (if (= (car l) 5)
          (+ 1 (count-5 (cdr l)))
          (count-5 (cdr l)))))

; sampling
(define experiment (repeat geometric-model n-samples))

; ratio between the number of samples with value 5
; and the total number of samples
(/ (count-5 experiment) n-samples)

; histogram of the results
(hist experiment)


; exercise 7
; definition of the model
(define (a-b-model)
  (define a
    (flip 0.8))
  (define b
    (if a
        (flip 0.5)
        (flip 0.3)))
  (list a b))

; sampling and visualization of the results
(hist (repeat a-b-model n-samples))