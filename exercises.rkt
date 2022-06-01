#lang gamble
(require gamble/viz)

; EXERCISE 1
; Exericse 1.4 in Structure and Interpretation of Computer Programs
(define (a-plus-abs-b a b) 
  ((if (> b 0) + -) a b))

; (a-plus-abs-b 10 -5)


; Exercise 1.5 in Structure and Interpretation of Computer Programs
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p))



; EXERCISE 2
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

; definition of transformation with average damping, i.e. x ->  (x + log(1000) / log(x)) / 2
(define (log-transformation-avg-dmp x)
  (average x (log-transformation x)))

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
  (if (= (remainder (- i 2) 3) 0)
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

; EXERCISE 3
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



; EXERCISE 4
; What are (bernoulli-dist p), (normal-dist μ σ) exactly? Are they real numbers (produced in a random way)?
; We have seen that flip is a procudere with a probabilistic behaviour. Is, e.g., (normal-dist μ σ) something similar?
; TRY TO EVALUATE (normal-dist 0 1)
(normal-dist 0 1)

; We have a new type of objects: distributions. To check if something is a distribution-object, we use the procedure dist?
; EXERCISE
; Evaluate
(dist? (normal-dist 0 1))
(dist? (bernoulli-dist 0.5))
(dist? flip)
; What is the difference between flip and (bernoulli-dist 0.5)?



; EXERCISE 5
; NOTE: the follwing one is a Church program, so it has to be executed with a Church interpreter

; POINT D
; definition of the possible outcomes
(define x '(red blue green black))
; definition of the probability for each outcome
(define x-weights '(0.5 0.05 0.4 0.05))

; equivalent definition of the x-weights
; (define x-weights '(5 0.5 4 0.5))

; definition of a model which represents the distribution given in the exercise
(define (multinomial-model)
  (multinomial x x-weights))

; visualization of the results
(hist (repeat 10000 multinomial-model))


; POINT E
;; define some variables and utility functions
(define letters '(a b c d e f g h i j k l m n o p q r s t u v w x y z) )
(define (vowel? letter) (if (member letter '(a e i o u y)) #t #f))
(define letter-probabilities (map (lambda (letter) (if (vowel? letter) 0.01 0.047)) letters))

(define (my-list-index needle haystack counter)
  (if (null? haystack)
      'error
      (if (equal? needle (first haystack))
          counter
          (my-list-index needle (rest haystack) (+ 1 counter)))))

(define (get-position letter) (my-list-index letter letters 1))

;; actually compute p(h | win)
(define distribution
  (enumeration-query
   (define my-letter
     (multinomial letters letter-probabilities))

   (define my-position (get-position my-letter))
   (define my-win-probability
     (/ 1.0 (* my-position my-position)))
   (define win? (if (equal? my-letter 'a) 
                    #t 
                    (flip my-win-probability)))

   ; query to get the probability P(h | win)
   my-letter

   ; query to get the probabilities P(vowel | win) and P(consonant | win)
   ; (if (vowel? my-letter) 'vowel 'consonant) 

   ;; condition
   win?
   ))

(barplot distribution)

; EXERCISE 6
; To see the problems of rejection sampling, consider the following
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