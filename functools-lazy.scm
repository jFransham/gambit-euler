(load "functools")
(include "macros.scm")

(define car-lazy car)
(define cdr-lazy (compose force cdr))

(define (filter-lazy pred? lst)
  (if (null? lst)
      '()
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (if (pred? fst)
            (cons-lazy fst (filter-lazy pred? (force snd)))
            (filter-lazy pred? (force snd))))))

(define find-lazy (compose car filter-lazy))

(define (map-lazy f lst)
  (if (null? lst)
      '()
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (cons-lazy (f fst) (map-lazy f (force snd))))))

(define (list->lazy-list lst)
  (if (null? lst)
      '()
      (cons-lazy (car lst) (list->lazy-list (cdr lst)))))

;; TODO: Replace this with Sieve of Atkin in Project Euler #10 (since it is
;;       strict, perhaps do it in chunks?)
(define primes-lazy
  (cons-lazy
   2
   (cons-lazy
    3
    (let inner ([primes '(2 3)] [cur 5])
      (if (not (any (lambda (x) (= 0 (remainder cur x)))
                    primes))
          (cons-lazy cur (inner (cons cur primes) (+ cur 2)))
          (inner primes (+ cur 2)))))))

(define (uniq-lazy lst)
  (let inner ([acc '()] [lst lst])
    (cond
     [(null? lst) '()]
     [(contains (car lst) acc)
      (inner acc (cdr-lazy lst))]
     [else
      (cons-lazy
       (car lst)
       (inner (cons (car lst) acc)
              (cdr-lazy lst)))])))

(define nats-lazy
  (let nats-from ([n 0])
    (cons-lazy n (nats-from (+ n 1)))))

(define (range-lazy lo hi)
  (range-step-lazy lo hi (if (< lo hi) 1 -1)))

(define (range-step-lazy lo hi step)
  (let ([op (if (< step 0) <= >=)])
    (range-step-op-lazy lo hi step op)))

(define (range-step-op-lazy lo hi step op)
  (if (op lo hi)
      '()
      (cons-lazy lo (range-step-op-lazy (+ lo step)
                                        hi
                                        step
                                        op))))

(define (range-inclusive-lazy lo hi)
  (range lo (if (< lo hi)
                (+ hi 1)
                (- hi 1))))

(define (last-lazy lst)
  (let ([snd ((cdr lst))])
    (if (null? snd)
        (car lst)
        (last snd))))

(define (take-lazy n lst)
  (if (= n 0)
      '()
      (cons-lazy (car-lazy lst)
                 (take-lazy (- n 1) (cdr-lazy lst)))))

(define (take-while-lazy pred? lst)
  (if (null? lst)
      lst
      (let ([fst (car-lazy lst)]
            [snd (cdr-lazy lst)])
        (if (pred? fst)
            (cons-lazy fst (take-while-lazy pred? snd))
            '()))))

(define (skip-while-lazy pred? lst)
  (if (null? lst)
      lst
      (if (pred? (car-lazy lst))
          (skip-while-lazy pred? (cdr-lazy lst))
          lst)))

(define (foldl-lazy f default lst)
  (if (null? lst)
    default
    (let ([fst (car lst)]
          [snd (cdr lst)])
      (f fst (foldl-lazy f default (force snd))))))

(define (foldl1-lazy f lst)
  (let ([fst (car lst)]
        [snd (cdr-lazy lst)])
    (if (null? snd)
        fst
        (f fst (foldl1-lazy f snd)))))

(define all-lazy
  (compose (curry foldl-lazy ∧ #t) map-lazy))

(define any-lazy
  (compose (curry foldl-lazy ∨ #f) map-lazy))

(define (max-lazy lst)
  (let inner ([cur-max (car lst)] [rest (cdr lst)])
    (let ([fr (force rest)])
      (if (null? fr)
          cur-max
          (let ([cur (car fr)])
            (if (> cur cur-max)
                (inner cur     (cdr fr))
                (inner cur-max (cdr fr))))))))

(define (max-by-lazy fn lst)
  (let inner ([cur-max (car lst)] [rest (cdr lst)])
    (let ([fr (force rest)])
      (if (null? fr)
          cur-max
          (let ([cur (car fr)])
            (if (> (fn cur) (fn cur-max))
                (inner cur     (cdr fr))
                (inner cur-max (cdr fr))))))))

(define (append-lazy fst . lsts)
  (if (null? lsts)
      fst
      (if (null? fst)
          (apply append-lazy lsts)
          (cons-lazy (car fst)
                     (apply append-lazy
                            (force (cdr fst))
                            lsts)))))

(define (cross-product-lazy fst snd)
  (define (inner lst elem)
    (map-lazy (curry list elem)
              lst))
  (apply-append-lazy (map-lazy (curry inner snd)
                               fst)))

(define (apply-append-lazy lsts)
  (let ([fst  (car-lazy lsts)]
        [rest (cdr-lazy lsts)])
    (if (null? rest)
        fst
        (if (null? fst)
            (apply-append-lazy rest)
            (cons-lazy (car fst)
                       (apply-append-lazy
                        (cons-lazy (cdr-lazy fst)
                                   rest)))))))

(define (nth-lazy n lst)
  (if (= n 0)
      (car lst)
      (nth-lazy (- n 1)
                (force (cdr lst)))))

(define (lazy-list->list lst)
  (foldl-lazy cons '() lst))


(define (sum-lazy lst)
  (foldl-lazy + 0 lst))
