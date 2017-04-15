;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;
;; Find the sum of all the primes below two million.

(load "functools-lazy")
(include "macros.scm")

(define (solutions-4x^2+y^2 limit)
  (define search-space
    (cross-product-lazy (range-lazy 1      ; All x's
                                    (sqrt (/ (- limit 1) 4)))
                        (range-step-lazy 1 ; Odd y's
                                         (sqrt (- limit 4))
                                         2)))
  (filter-lazy
   (curry >= limit)
   (map-lazy
    (curry apply
           (λ (x y) (+ (* 4 (square x))
                       (square y))))
    search-space)))

(define (solutions-3x^2+y^2 limit)
  (define search-space
    (cross-product-lazy (range-step-lazy 1 ; Odd x's
                                         (sqrt (/ (- limit 1) 3))
                                         2)
                        (range-step-lazy 2 ; Even y's
                                         (sqrt (- limit 3))
                                         2)))
  (filter-lazy
   (curry >= limit)
   (map-lazy
    (curry apply
           (λ (x y) (+ (* 3 (square x))
                       (square y))))
    search-space)))

(define (solutions-3x^2-y^2 limit)
  ;; Solves quadratic 3x² - (x - 1)² = limit (i.e. the x value that gives the
  ;; maximum possible value of 3x² - y²)
  (define max-x (/ (- (sqrt (- (* 2 limit) 1)) 1) 2))
  (define search-space
    (apply-append-lazy
     (map-lazy (λ (x)
                 (map-lazy (curry list x)
                           (range-step-lazy (- x 1) 0 -2)))
               (range-lazy 2 max-x))))
  (filter-lazy
   (curry >= limit)
   (map-lazy
    (curry apply
           (λ (x y) (- (* 3 (square x))
                       (square y))))
    search-space)))

;; Sieve of Atkin, see https://en.wikipedia.org/wiki/Sieve_of_Atkin
(define (primes-below n)
  ;; For performance reasons we use constant-time indexable `vector' instead of
  ;; list, and set it mutably instead of purely.
  ;; We waste space for 0, 1, 2, 3, etc. (which we will never need to check),
  ;; but it makes the implementation more readable.
  (define possible-solutions (make-vector n #f))
  (define is-prime? (curry vector-ref possible-solutions))
  (define set-prime! (curry vector-set! possible-solutions))
  (define (flip! n) (set-prime! n (not (is-prime? n))))
  (define wheel-hits '(1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59))
  (define check-nums
    (take-while-lazy
     (curry >= n)
     (map-lazy
      (λ (pair) (+ (* 60 (car pair))
                   (cadr pair)))
      (cross-product-lazy
       (range-lazy 0 (/ n 60))
       (list->lazy-list wheel-hits)))))
  (for-lazy [i (solutions-4x^2+y^2 n)]
            (case (remainder i 60)
              [(1 13 17 29 37 41 49 53)
               (flip! i)]))
  (for-lazy [i (solutions-3x^2+y^2 n)]
            (case (remainder i 60)
              [(7 19 31 43)
               (flip! i)]))
  (for-lazy [i (solutions-3x^2-y^2 n)]
            (case (remainder i 60)
              [(11 23 47 59)
               (flip! i)]))
  (for-lazy [i (filter-lazy
                (λ (x)
                  (and (>= x 7)
                       (is-prime? x)))
                check-nums)]
            (define not-prime
              (take-while-lazy (curry >= n)
                               (map-lazy (curry * i i)
                                         check-nums)))
            (for-lazy [j not-prime]
                      (set-prime! j #f)))
  (list* 2 3 5
         (lazy-list->list
          (filter-lazy is-prime? (range-lazy 7 n)))))

(displayln (foldl1 + (primes-below 2000000)))
