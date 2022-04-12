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
  (define (cont-frac-rec k counter) 
    (if (= counter k)
        (/ (n counter) (d counter))
        (/ (n counter)
           (+ (d counter) 
              (cont-frac-rec k (+ counter 1))))))
  (cont-frac-rec k 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           number-of-iterations)

; cont-frac iterative process
(define (cont-frac-iter n d k)
  (define (iter k res)
    (if (= k 1)
        (/ (n k) res)
        (iter (- k 1)
              (+ (d (- k 1))
                 (/ (n k) res)))))
  (iter k (d k)))


; exercise 1.38
(define (get-n i) 1)
(define (get-d i)
  (if (= (modulo (- i 2) 3) 0)
      (* (+ (quotient
             (- i 2)
             3)
            1)
         2)
      (if (<= i 2) i 1)))

(define (euler-number k)
  (+ (cont-frac get-n get-d k) 2))
(define (euler-number-iter k)
  (+ (cont-frac-iter get-n get-d k) 2))

(euler-number number-of-iterations)
(euler-number-iter number-of-iterations)