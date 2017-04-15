; 2520 is the smallest number that can be divided by each of the numbers from 1
; to 10 without any remainder.
;
; What is the smallest positive number that is evenly divisible by all of the
; numbers from 1 to 20?

(load "functools")
(include "macros.scm")

(define/memo (factors n)
  (let loop ([i (inexact->exact (floor (sqrt n)))])
    (if (= i 1)
        #f
        (if (= 0 (remainder n i))
          (cons i (/ n i))
          (loop (- i 1))))))

(define (lowest-common-multiple . args)
  (define not-1 (filter (compose not (curry = 1))
                        args))
  (define with-factors (map (λ (x) (cons x (factors x))) not-1))
  (define facs (apply append
                      (map (λ (x) (list (car x) (cdr x)))
                           (filter identity (map cdr with-factors)))))
  (define check-args (filter (λ (x) (not (contains x facs)))
                             args))
  (define primes (map car (filter (compose not cdr) with-factors)))
  (define step    (max check-args))
  (define initial (apply * (cons step primes)))
  (let inner ([multiple initial])
    (if (all (compose (curry = 0)
                      (curry remainder multiple))
             check-args)
        multiple
        (inner (+ multiple step)))))

(displayln (apply lowest-common-multiple (range 1 21)))
