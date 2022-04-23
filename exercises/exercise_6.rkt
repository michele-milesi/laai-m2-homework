#lang gamble
(require gamble/viz)

; EXERCISE. To see the problems of rejection sampling, consider the following
; variation of the previous example:

; Try to see what happens when you lower the basesate.
; What happens if we set it to 0.01?
; And to 0.001?

(define (model baserate)
  (define (take-sample)
    (rejection-sampler
     (define A (if (flip baserate) 1 0))
     (define B (if (flip baserate) 1 0))
     (define C (if (flip baserate) 1 0))
     (define D (+ A B C))
     (observe/fail (>= D 2))
     A))
  (take-sample))

; experiment with baserate = 0.1
(hist (repeat (model 0.1) 1000))

; experiment with baserate = 0.01
(hist (repeat (model 0.01) 1000))

; experiment with baserate = 0.001
(hist (repeat (model 0.001) 1000))
