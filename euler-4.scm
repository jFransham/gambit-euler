; A palindromic number reads the same both ways. The largest palindrome made
; from the product of two 2-digit numbers is 9009 = 91 × 99.
;
; Find the largest palindrome made from the product of two 3-digit numbers.

(include "macros.scm")
(load "functools")
(load "functools-lazy")

(define (palindrome? n)
  (define magnitude
    (inexact->exact (floor (log-base 10 n))))
  (define half-mag (floor (/ magnitude 2)))
  (let inner ([i 0])
    (or (> i half-mag)
        (and (= (get-digit n i)
                (get-digit n (- magnitude i)))
             (inner (+ i 1))))))

(define numbers-3-digits (range 100 999))

(displayln
 (max-lazy
  (filter-lazy palindrome?
               (map-lazy
                (curry apply *)
                (list->lazy-list
                 (cross-product numbers-3-digits
                                numbers-3-digits))))))
