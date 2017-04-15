; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
; get 3, 5, 6 and 9. The sum of these multiples is 23.
;
; Find the sum of all the multiples of 3 or 5 below 1000.

(load "functools")

(define (is-mult? a b)
  (eq? (remainder a b) 0))

;; TODO: Reduce to prime factors first
(define (get-repeating-mult-pattern . args)
  (filter (lambda (x) (any (lambda (y) (is-mult? x y)) args))
          (range 1 (apply * args))))

(define (multiples-to n . nums)
  (define mul-pat (apply get-repeating-mult-pattern nums))
  (define mul-pat-0 (cons 0 mul-pat))
  (define step (+ (car mul-pat) (last mul-pat)))
  (define pat-len (length mul-pat))
  (define (multiples-inner start)
    (let* ([cur-pat (map (curry + start) mul-pat-0)])
      (if (> (+ start step) n)
          (take-while (curry > n) cur-pat)
          (append cur-pat
                  (multiples-inner (+ start step))))))
  (multiples-inner 0))

(define (naive-multiples-to n . nums)
  (filter (lambda (x) (any (lambda (y) (is-mult? x y)) nums))
          (range 0 n)))

(displayln (apply + (multiples-to 1000 3 5)))
