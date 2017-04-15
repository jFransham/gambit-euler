(include "macros.scm")

(define default-hash-capacity 10)

(define (make-hash . rest)
  (apply make-hash-internal eqv? eqv?-hash rest))

(define (make-hash-equal . rest)
  (define buckets (if (null? rest) default-hash-capacity (car rest)))
  (apply make-hash-internal equal? equal?-hash rest))

(define (make-hash-internal eq-pred? eq-pred?-hash . rest)
  (define buckets (if (null? rest) default-hash-capacity (car rest)))
  `(,eq-pred? ,eq-pred?-hash ,(make-vector buckets '())))

(define (hash-bucket hash key)
  (define eq-pred?-hash (cadr hash))
  (define hash-internal (caddr hash))
  (modulo (eq-pred?-hash key) (vector-length hash-internal)))

(define (hash-ref hash key . rest)
  (define default (if (null? rest) #f (car rest)))
  (define eq-pred? (car hash))
  (define hash-internal (caddr hash))
  (define out (find (lambda (x) (eq-pred? key (car x)))
                    (vector-ref hash-internal (hash-bucket hash key))
                    '()))
  (if (null? out)
      default
      (cdr out)))

(define (hash-set hash key value)
  (define out-hash (vector-copy hash))
  (hash-set! out-hash key value)
  out-hash)

(define (hash-set! hash key value)
  (define bucket (hash-bucket hash key))
  (define hash-internal (caddr hash))
  (define cur (vector-ref hash-internal bucket))
  (vector-set! hash-internal
               bucket
               (cons (cons key value) cur))
  hash)

(define (∧ fst . args)
  (and fst
       (if (null? args)
           #t
           (apply ∧ args))))

(define (∨ fst . args)
  (or fst
      (if (null? args)
          #f
          (apply ∨ args))))

(define (identity x) x)

(define (compose f . rest)
  (if (null? rest)
      f
      (λ args
        (f (apply (apply compose rest)
                  args)))))

(define (cross-product fst snd)
  (define (inner f lst elem)
    (map (f list elem)
         lst))
  (apply append (map (curry inner curry snd) fst)))

(define (foldl1 f lst)
  (let ([fst (car lst)]
        [snd (cdr lst)])
    (if (null? snd)
        fst
        (f fst (foldl1 f snd)))))

(define (any pred? lst)
  (if (null? lst)
      #f
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (or (pred? fst)
            (any pred? snd)))))

(define (all pred? lst)
  (if (null? lst)
      #t
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (and (pred? fst)
             (all pred? snd)))))

(define (find pred? lst default)
  (if (null? lst)
      default
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (if (pred? fst)
            fst
            (find pred? snd default)))))

(define (uniq lst)
  (let inner ([acc '()] [lst lst])
    (cond
     [(null? lst) acc]
     [(contains (car lst) acc)
      (inner acc (cdr lst))]
     [else
      (inner (cons (car lst) acc)
             (cdr lst))])))

(define (contains elem lst)
  (define uniq '(()))
  (not (eq? uniq (find (curry = elem) lst uniq))))

(define (filter pred? lst)
  (if (null? lst)
      lst
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (if (pred? fst)
            (cons fst (filter pred? snd))
            (filter pred? snd)))))

(define (take-while pred? lst)
  (if (null? lst)
      lst
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (if (pred? fst)
            (cons fst (take-while pred? snd))
            '()))))

(define (skip-while pred? lst)
  (if (null? lst)
      lst
      (let ([fst (car lst)]
            [snd (cdr lst)])
        (if (pred? fst)
            (skip-while pred? snd)
            lst))))

(define (skip-until pred? lst)
  (skip-while (lambda (x) (not (pred? x))) lst))

(define (displayln a)
  (display a)
  (newline))

(define (curry f . curried-args)
  (lambda args (apply f (append curried-args args))))

(define (rcurry f . curried-args)
  (lambda args (apply f (append args curried-args))))

(define (max lst)
  (let inner ([cur-max (car lst)] [rest (cdr lst)])
    (if (null? rest)
        cur-max
        (let ([cur (car rest)])
          (if (> cur cur-max)
              (inner cur     (cdr rest))
              (inner cur-max (cdr rest)))))))

(define (min lst)
  (let inner ([cur-max (car lst)] [rest (cdr lst)])
    (if (null? rest)
        cur-max
        (let ([cur (car rest)])
          (if (< cur cur-max)
              (inner cur     (cdr rest))
              (inner cur-max (cdr rest)))))))

(define (range lo hi)
  (range-step lo hi (if (< lo hi) 1 -1)))

(define (range-step lo hi step)
  (let ([op (if (< step 0) <= >=)])
    (range-step-op lo hi step op)))

(define (range-step-op lo hi step op)
  (if (op lo hi)
      '()
      (cons lo (range-step-op (+ lo step)
                              hi
                              step
                              op))))

(define (range-inclusive lo hi)
  (range lo (if (< lo hi)
                (+ hi 1)
                (- hi 1))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (take n lst)
  (if (= n 0)
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define sum (curry apply +))

(define prime-factors
  (lambda/memo (n)
               (define (first-factor init)
                 (cond
                  [(= init 1)               #f]
                  [(= (remainder n init) 0) init]
                  [else                     (first-factor (- init 1))]))
               (define factor (first-factor (inexact->exact (floor (sqrt n)))))
               (if factor
                   (append (prime-factors factor) (prime-factors (/ n factor)))
                   (list n))))

(define (get-digit number digit)
  (modulo (inexact->exact
           (floor (/ number
                     (expt 10 digit))))
          10))

(define (log-base n k)
  (/ (log k) (log n)))

(define (*** f . args)
  (if (null? args)
      (lambda args (list (apply f args)))
      (let ([rest-fn (apply *** args)])
        (lambda args
          (cons (apply f args)
                (apply rest-fn args))))))
