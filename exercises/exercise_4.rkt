#lang gamble

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