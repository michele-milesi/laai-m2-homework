#lang gamble
(require gamble/viz)

(define sum (λ (xs) (foldl + 0 xs)))

(define model1 (rejection-sampler
     (define A (bernoulli 0.5))
     (define B (bernoulli 0.5))
     (define C (bernoulli 0.5))
     (define D (+ A B C))
     (observe/fail (= A 1))
     D))

 ;(hist (repeat model1 100))


(define model2
  (rejection-sampler
   (define A (bernoulli 0.5))
   (define B (bernoulli 0.5))
   (define C (bernoulli 0.5))
   (define D (+ A B C))
   (observe/fail (>= D 2))
   A
   ))

; (hist (repeat model2 100))

(define model3
  (rejection-sampler
   (define is-weekend (bernoulli (/ 2 7)))
   (define rate (if is-weekend
                    3
                    10))
   (observe-sample (poisson-dist rate) 4)
   is-weekend))

;(hist (repeat model3 100))

(define (model3-rejection)
   (define is-weekend (bernoulli (/ 2 7)))
   (define rate (if is-weekend 3 10))
   (define observed-value (sample (poisson-dist rate)))
   (if (= observed-value 4)
       is-weekend
       (model3-rejection)))

; Rejection sampling is conceptually appealing, but it has two major problems.
; 1) We have to be sure that stochastic recursion (almost surely) terminates
; 2) Even if terminates, it is usually not efficient

; EXERCISE. To see the problems of rejection sampling, consider the following
; variation of the previous example:

(define baserate 0.1)
(define (take-sample)
  (rejection-sampler
   (define A (if (flip baserate) 1 0))
   (define B (if (flip baserate) 1 0))
   (define C (if (flip baserate) 1 0))
   (define D (+ A B C))
   (observe/fail (>= D 2))
   A))

; Try to see what happens when you lower the basesate.
; What happens if we set it to 0.01?
; And to 0.001?

; EXPLICIT COMPUTATION

; Another option is to apply Bayes' rule and explicitly compute the posterior probability.
; We can do so using the sampler enumerate.

(enumerate
   (define A (bernoulli 0.5))
   (define B (bernoulli 0.5))
   (define C (bernoulli 0.5))
   (define D (+ A B C))
   (observe/fail (>= D 2))
   A)

; enumerate produces a (discrete) distributions of the form [V1 .. Vn] [ε1 .. εn]
; meaning that the posterior distribution assigns to value Vi probability εi.
; Here is another example

(enumerate
 (define A (if (flip baserate) 1 0))
 (define B (if (flip baserate) 1 0))
 (define C (if (flip baserate) 1 0))
 (define D (+ A B C))
 (observe/fail (>= D 2))
 A)

; enumerate works for (discrete) toy models only. In fact, enumerate works as follows: it explores all
; paths in the probability tree to obtain all possible values of A, and then use the laws of
; probability to compute the posterior. If we have a model with just coin flips, then for every sampling
; we have 2 new paths to consider. Therefore, if we have n flips, we have 2^n paths, meaning that
; enumerate grows exponentially in the number of samplings. 
   
(define model1-mh
  (mh-sampler
     (define A (bernoulli 0.5))
     (define B (bernoulli 0.5))
     (define C (bernoulli 0.5))
     (define D (+ A B C))
     (observe/fail (>= D 2))
     A))

; EXAMPLE: TUG-OF-WAR

; Recall the tug-of-war example. We now do some inference on it asking, e.g., how likely is
; that Bob is strong, given that he has been in a series of winning teams.

(define tug-of-war
  (mh-sampler
   (define strength (mem (lambda (person) (normal 0 1))))
   (define lazy (lambda (person) (flip (/ 1 3))))

   (define (pulling person) (if (lazy person) (/ (strength person) 2) (strength person)))
   (define (total-pulling team) (sum (map pulling team)))

   (define (winner team1 team2) (if (< (total-pulling team1) (total-pulling team2)) 'team2 'team1))

   (observe/fail (and (eq? 'team1 (winner '(bob mary) '(tom sue)))
                      (eq? 'team1 (winner '(bob sue) '(tom jim)))))

   (strength 'bob)))

; (density (repeat tug-of-war 1000))


;-------------------
; LEARNING
;-------------------

; We have argued that both "learning" and "reasoning" can be seen as
; instances of conditional inference.
; We now take a closer look to learning.
; We formulate learning as inference in a model such that
; (1) It has a fixed, latent variable H (the hypotheis)
; (2) Has a sequene of observation, the data points.
; Let us see an example.

; A friend gives you a coin. Yo don't know whether the coin is fair or not, but you have no reason
; to believe the coin to be unfair. We model that by defining your prior as giving probability
; 1/1000 that the coin is unfair. 
; You flip it five times, and the result is (H H H H H).
; This may seem as a nice coincidence, but not enough to conclude that the coin is unfair.
; You repeat the experiment, this time obtaining (H H H H H H H H H H). And then again (H H H H H H H H H H H H H H H).
; Now you are starting to have enough data to learn that the coin is unfair...No matter what your prior was,
; if you are rational, then you infer the coin to be a tricked one.

(define observed-data '(h h h h h))
(define make-coin (λ (weight) (λ () (if (flip weight) 'h 't))))

(define (learning1 data)
  (mh-sampler
   (define fair-coin? (flip 0.999))  ; prior: with high probability the coin is fair
   (define coin (make-coin (if fair-coin? 0.5 0.95)))
   (observe/fail (equal? data (repeat coin (length data))))  ; we condition our knowledge on data.
   fair-coin? ; posterior
   ))

;(hist (repeat (learning1 observed-data) 1000))
; With only 5 heads, you still firmly believe the coin to be fair

; (define observed-data-10 '(h h h h h h h h h h))
; (hist (repeat (learning1 observed-data-10) 5000))
; With 10 heads, you suspect the coin to be tricked, although you still believe it to be fair.
; The probabilities that the coin is fair/unfair are comparable, but fairness is more likely

(define observed-data-15 '(h h h h h h h h h h h h h h h h))
(hist (repeat (learning1 observed-data-15) 1000))
; With 15 heads, you are almost sure the coin is tricked

; What we are facing here is the so called "learning curve", namely how learning depend on data.

; Previous example is a simple case of learning, where one has just 2 hypotheses.
; In ML, this is almostnever the case. Typically, one has either countably many hypotheses
; (this happens when dealing with discrete and symbolic knowledge, e.g. language and grammar)
; or even continuous hypothesis spaces (e.g. in vision, perception, motion, etc...)
; To see an example of continuous learning, we modify our hypothesis generator make-coin by making its
; weight changing according to the uniform distribution over [0,1].
; What we want to learn is not whether the coin is fair or not, but its weight (which belongs to [0,1]),
; assuming as a prior that the latter is uniformly distributed
; 

(define (learning-cont data)
  (mh-sampler
   (define coin-weight (uniform 0 1)) ; prior: the weight of the coin is uniformly distributed
   (define coin (make-coin coin-weight))
   (observe/fail (equal? data (repeat coin (length data))))  ; we condition our knowledge on data.
   coin-weight ; posterior
   ))

"prior knoweldge"
(define prior-learning (repeat (λ () (uniform 0 1)) 1000))
(density prior-learning)

"poterior knowledge after data"
(density (repeat (learning-cont observed-data) 3000))

; ARITHMETIC EXPRESSIONS
; One problem Bayesian modelling suffers is the hypothesis space dimension.
; Looking at the examples seen so far, we see that hypothesis spaces are either too
; small or ad hoc (or both). Oftentimes, real-worlds scenarios involve large (possibly infinite)
; hypothesis spaces. How can we come with those, having finite tools only?
; In this example, we see how we can build large hypothesis spaces by defining a simple
; hypothesis language whose grammar generates infinitely many potential hypotheses.

; We define a grammar for simple arithmetic expressions.
; e ::= x | n | (+ e e) | (- e e)

(define sample-integer
  (discrete-dist [0 1/10] [1 1/10] [2 1/10] [3 1/10] [4 1/10] [5 1/10] [6 1/10] [7 1/10] [8 1/10] [9 1/10]))

(define (random-arithmetic-expression)  ; notice that random-arithmetic-expression is stochastic recursive procedure with 0 parameters
  (if (flip 0.7)
      (if (flip) 'x (sample sample-integer))
      (list (sample (discrete-dist ['+ 1/2] ['- 1/2])) (random-arithmetic-expression) (random-arithmetic-expression))))

;(repeat random-arithmetic-expression 15)

; Given an expression exp, we now define a function to evaluate such an expression. The result will be something
; of the form λ (x) exp, so that by passing an actual parameter a for x, we can compute the integer represented by exp

(define (exp-to-proc expr)
  (eval (list 'lambda '(x) expr)))

; We now move to inference. Our goal is to find an expression on variable x such that the expression evaluates to 3
; when x is 1. Expressions satisfying this requirements are, e.g.: 3, (+ x 2), (+ 1 2), (+ x (- x 3)), ...

(define arithmetic-model
  (rejection-sampler
   (define expr (random-arithmetic-expression))
   (define proc (exp-to-proc expr))
   (observe/fail (= (proc 1) 3))
   expr))