; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
; a2 + b2 = c2
;
; For example, 32 + 42 = 9 + 16 = 25 = 52.
;
; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
; Find the product abc.

(load "functools-lazy")
(include "macros.scm")

(define (decreasing-cross-self lst)
  (define (inner val rest)
    (map (curry cons val) rest))
  (let loop ([lst lst])
    (if (null? lst)
        '()
        (append (inner (car lst) (cdr lst))
                (loop  (cdr lst))))))

(define (decreasing-cross-self-lazy lst)
  (define (inner val rest)
    (map-lazy (curry cons val) rest))
  (let loop ([lst lst])
    (if (null? lst)
        '()
        (append-lazy (inner (car-lazy lst) (cdr-lazy lst))
                     (loop  (cdr-lazy lst))))))

(define (pythagorean-triplet x y)
  (let* ([sum-squares (+ (square x) (square y))]
         [sqrt-sum    (sqrt sum-squares)])
    (if (exact? sqrt-sum)
        sqrt-sum
        #f)))

(define (find-pair-with-pythagorean-sum sum)
  (find-lazy
   (λ (x)
     (let ([trip (pythagorean-triplet (car x) (cdr x))])
       (and trip (= 1000 (+ trip (car x) (cdr x))))))
   (decreasing-cross-self-lazy
    (range-lazy 1
                (inexact->exact sum)))))

(define pair
  (find-pair-with-pythagorean-sum 1000))

(displayln (* (car pair)
              (cdr pair)
              (pythagorean-triplet
               (car pair)
               (cdr pair))))
