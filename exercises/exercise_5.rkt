#lang gamble
(require math/number-theory)

; NOTE: the follwing one is a Church program, so it has to be executed with a Church interpreter

;; define some variables and utility functions
(define letters '(a b c d e f g h i j k l m n o p q r s t u v w x y z) )
(define (vowel? letter) (if (member letter '(a e i o u y)) #t #f))
(define letter-probabilities (map (lambda (letter) (if (vowel? letter) 0.01 0.047)) letters))

(define (my-list-index needle haystack counter)
  (if (null? haystack)
      'error
    (if (equal? needle (car haystack))
        counter
      (my-list-index needle (cdr haystack) (+ 1 counter)))))

(define (get-position letter) (my-list-index letter letters 1))

;; actually compute p(h | win)
(define distribution
  (enumerate  ; to check
   (define my-letter (multinomial letters letter-probabilities))

   (define my-position (get-position my-letter))
   (define my-win-probability (/ 1.0 (* my-position my-position)))
   my-win-probability ;to do
   (define win? (if (equal? my-letter 'a) 
                    #t 
                    (flip my-win-probability)))

   ;; query to get the probability P(h | win)
   my-letter

   ; query to get the probability P(vowel | win) and P(consonant | win)
   ; (if (vowel? my-letter) 'vowel 'consonant) 

   ;; condition
   win?
   ))

; (barplot distribution)