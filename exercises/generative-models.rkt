#lang gamble
(require gamble/viz)

; From now on, we work with the extension of Racket called 'gamble'.
; You can find gamble's documentation here: https://rmculpepper.github.io/gamble/
; You can also take a look at gamble's github page: https://github.com/rmculpepper/gamble
; there, you also find instruction on how to install gamble. In short, you can install gamble
; directly from DrRacket. Go to:
; file -> Install package
; then insert git://github.com/rmculpepper/gamble as Package source (see https://github.com/rmculpepper/gamble for details)
; WARNING: gamble's installation may go wrong becuase you miss some packages. If so, look in the error message
; what packages you need and install them following the above procedure (just replace git://github.com/rmculpepper/gamble
; with the address of the package you are interested in



; We add to the language primitives for random computations.
; the procedure flip is the most simple such a primitive. Evaluating
; flip multiple times, we see that sometimes we obtain #t and sometimes #f.
; To see the probabilistic behaviour of flip, we execute it multiple times

(define test-flip (repeat flip 1000))

; and collect the result in a histogram
(hist test-flip)

; flip is a *procedure* whose evaluation is probabilistic. To produce more general
; probabilistic behaviours, we add to the language primitives for actual distributions.

; bernoulli-dist p
; PRE: p in [0,1]
(define unfair-coin (bernoulli-dist 0.3))

; gamma-dist shape scale
; PRE: shape, scale > 0
(define my-gamma (gamma-dist 0.9 0.5))

; normal-dist mean variance
; PRE: variance > 0
(define my-gauss (normal-dist 0 1))

; What are (bernoulli-dist p), (normal-dist μ σ) exactly? Are they real numbers (produced in a random way)?
; We have seen that flip is a procudere with a probabilistic behaviour. Is, e.g., (normal-dist μ σ) something similar?
; TRY TO EVALUATE (normal-dist 0 1)

; We have a new type of objects: distributions. To check if something is a distribution-object, we use the procedure dist?
; EXERCISE
; Evaluate
; (dist? (normal-dist 0 1))
; (dist? (bernoulli-dist 0.5))
; (dist? flip)

; What is the difference between flip and (bernoulli-dist 0.5)?

; To produce probabilistic behaviours from a distribution we use the proedure sample.
; When we evaluate (sample μ), we obtain a value sampled from μ.
; The proedure flip is nothing but (λ () (sample (bernoulli-dist 0.5))), with 0 in place of #f and 1 in place of #t.

; As a syntactic sugar, we can replace expressions of the form
; (sample (name-dist p1 .. pN))
; with
; (name p1 .. pN)
; Thus, we write (normal μ σ) in place of (sample (normal μ σ))

; As we use histrograms to collect the results of multiple samplings from *discrete* distributions,
; we use (density list-of-results) for continuous ones.

;(density (repeat (λ () (normal 0 1)) 600))

; Examples of probabilistic functions
; 1. Fair coin
(define fair-coin (λ () (if (flip 0.5) 'h 't))) ;the thunk is a fair coin
;(hist (repeat fair-coin 20))

; 2. We make the coin unfair (e.g. 95% of times head)
(define trick-coin (λ () (if (flip 0.95) 'h 't)))

; We can make all of that more abstract, and define a function that generates coins, given in input a weight

(define (make-coin wgt) (λ () (if (flip wgt) 'h 't)))

; We now combine HO- and probabilistic features. We first consider an fair coin
(define my-coin (λ () (flip 0.5)))

; What we want to do now, is to flip the coin 10 times, count how many times we got head.
; This is a probabilistic experiment, whose outcome is a number. 
; We then repeat the experiment several times, this way obtaining a probabilistic model
; which results in a distribution over [0, .., 10]. Since the coin is fair, we expect to
; obtain a uniform distribution

(define sum (λ (xs) (foldl + 0 xs)))

(define (experiment n)
  (repeat (λ () (sum (map (λ (x) (if x 1 0)) (repeat my-coin 10)))) n))

; More structured code is as follows:
 (define (expriment1 n)
   (define toss (repeat my-coin 10))
   (define (bool-to-int x) (if x 1 0))
   (repeat (λ () (sum (map bool-to-int toss))) n))

; STOCHASTIC RECURSION

; We can write stochastic recursive functions that randomly decide whether to stop.
; For example, we can generate geometric distribution as a function that flips a (weighted) coin, returning 
; N -1 if the first true is on the Nth flip (that is, we return the number of times we get false before our first true):

(define (geometric p)
  (if (flip p)
      0
      (+ 1 (geometric p))))

(hist (repeat (λ () (geometric 0.5)) 300))

; In principle, (geometric 0.5) may loop forever. However, what we see is that
; this happens with probability 0. Programs terminating with probability 1 are
; said to be almost sure terminating. Here is another examples:

(define Ip (λ ()
             (if (flip)
                 (λ (x) (x))
                 (Ip))))


; MEMOIZATION

; First, let us introduce the construct for building discrete distributions

(define colors (discrete-dist ['blue 1/3] ['green 1/3] ['brown 1/3]))
(hist (repeat (λ () (sample colors)) 300))

(define (eye-color person) (sample colors))

; let us consider the expression (eye-color 'Alice). Every time we run this program,
; we obtain a different eye color for Alice. This results in undesired results.

; (hist (repeat (λ () (equal? (eye-color 'Alice) (eye-color 'Alice))) 10))

; Sometimes we would like to assign values randomly, but once and for all.
; If we do not know the eye color of a person, say Alice, we can assign such a color
; randomly. However, once we have done that, we expect the eye color of Alice to be
; the same at every execution of the program. This is, unfortunately, not the case:
; try to run in the REPL (eye-color 'Alice) multiple times.
; We want to model random but *persistent* properties.
; We do so using *memoization*
; Memoization is a HO-function mem with the following semantics: when we evaluate (mem exp) we build an expression such that
; the first time exp is evaluated its value is recoreded, so that future calls of exp are replaced with such a value.
; This is useful in probabilistic modelling, where to obtain information on the (probabilistic) behaviour of programs
; we execute them multiple times (think about a generative model: every execution of the program gives an info about the
; model, and thus we execute the program n times to approximate the whole distrbution describing such a model).

; Let us see an example of memoization
(define mem-flip (mem flip))
(hist (repeat (λ () (equal? (mem-flip) (mem-flip))) 20))

; We can now represent the notion that eye color is random, but each person has a fixed eye color.

(define eye-color-mem (mem (λ (person) (sample colors))))
 (hist (repeat (λ () (equal? (eye-color-mem 'Alice) (eye-color-mem 'Alice))) 10))


; Extended Example: Bayesian Tug of War
; Consider a tug of war game. Each participant may be strong or weak, and we assume strength to be a continuous property
; normally distriubuted among population.

 (define strength (mem (lambda (person) (normal 0 1))))

; Additionally, each person may be lazy or not on each match. If a person is lazy they only pull with half their strength.
; We assume each person has a 25% chance of being lazy.

(define lazy (lambda (person) (flip 0.25)))

; The team that pulls hardest will win.

(define (pulling person)
  (if (lazy person) (/ (strength person) 2) (strength person)))

(define (total-pulling team) (sum (map pulling team)))

(define (winner team1 team2) (if (< (total-pulling team1) (total-pulling team2)) team2 team1))

(list "Tournament results:"
      (winner '(alice bob) '(sue tom))
      (winner '(alice bob) '(sue tom))
      (winner '(alice sue) '(bob tom))
      (winner '(alice sue) '(bob tom))
      (winner '(alice tom) '(bob sue))
      (winner '(alice tom) '(bob sue)))




(define model1 (rejection-sampler
     (define A (bernoulli 0.5))
     (define B (bernoulli 0.5))
     (define C (bernoulli 0.5))
     (define D (+ A B C))
     (observe/fail (= A 1))
     D))

 (hist (repeat model1 100))

