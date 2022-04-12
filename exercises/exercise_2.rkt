#lang racket

; exercise 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; definition of transformation x -> 1 + 1/x
(define (transformation x)
  (+ 1 (/ 1 x)))

; definition of golden ratio
(define phi (/ (+ 1 (sqrt 5)) 2))


; exercise 1.36
; modification of fixed-point procedure in order to be able to print the sequence 
; of approximations it generates
(define (fixed-point-print-seq f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; definition of transformation x -> log(1000) / log(x) without average damping
(define (log-transformation x)
  (/ (log 1000) (log x)))

; definition of procedure which computes the average
(define (average x y) 
  (/ (+ x y) 2))

; definition of transformation x -> log(1000) / log(x) with average damping
(define (log-transformation-avg-dmp)
  (fixed-point-print-seq
   (lambda (x)
     (average x (log-transformation x)))
   1.1))


; exercise 1.37
(define number-of-iterations 11)

; cont-frac recursive process
(define (cont-frac n d k)
  (if (= k 1)
      (/ (n (- number-of-iterations k)) (d (- number-of-iterations k)))
      (/ (n (- number-of-iterations k)) (+ (d (- number-of-iterations k)) (cont-frac n d (- k 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           number-of-iterations)

; cont-frac iterative process
(define (cont-frac-iter n d k res)
  (if (= k 0)
      (/ (n k) res)
      (cont-frac-iter n 
                      d 
                      (- k 1) 
                      (+ (d (- k 1)) 
                         (/ (n k) res)))))


; exercise 1.38
(define (get-n i) 1)
(define (get-d i)
  (if (= (modulo (- i 1) 3) 0)
      (* (+ (quotient (- i 1) 3) 1) 2)
      (if (< i 2) (+ i 1) 1)))

(cont-frac get-n get-d number-of-iterations)
(cont-frac-iter get-n get-d number-of-iterations (get-d number-of-iterations))