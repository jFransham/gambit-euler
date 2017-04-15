; The prime factors of 13195 are 5, 7, 13 and 29.
;
; What is the largest prime factor of the number 600851475143 ?

(load "functools")
(include "macros.scm")

(define/memo (prime-factors n)
  (define (first-factor init)
    (cond
     [(= init 1)               #f]
     [(= (remainder n init) 0) init]
     [else                     (first-factor (- init 1))]))
  (define factor (first-factor (inexact->exact (floor (sqrt n)))))
  (if factor
      (append (prime-factors factor) (prime-factors (/ n factor)))
      (list n)))

(define (largest-prime-factor n)
  (max (prime-factors n)))

(displayln (largest-prime-factor 600851475143))
