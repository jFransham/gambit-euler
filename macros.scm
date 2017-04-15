;; LaTeX is the future
(define-macro (λ . args)
  `(lambda ,@args))

(define-macro (cons-lazy a b)
  `(cons ,a (delay ,b)))

(define-macro (lambda/memo args . rest)
  `(let ([inner-map (make-hash-equal)]
         ; All nulls compare `eq?', but a list is unique
         [unique '(())]
         [inner-lambda (lambda ,args ,@rest)])
     (lambda args
       (let ([cur (hash-ref inner-map args unique)])
         (if (eq? cur unique)
             (let ([out (apply inner-lambda args)])
               (hash-set! inner-map args out)
               out)
             cur)))))

(define-macro (define/memo name-and-args . rest)
  `(define ,(car name-and-args)
     (lambda/memo ,(cdr name-and-args) ,@rest)))

;; Imperative foreach (only allows a single variable, not multiple like
;; Racket's)
(define-macro (for var . rest)
  (let ([name      (car  var)]
        [list-expr (cadr var)])
    `(let ([--list-- ,list-expr])
       (let --loop-- ([--list-- --list--])
         (if (null? --list--)
             (void)
             (let ([,name (car --list--)])
               ,@rest
               (--loop-- (cdr --list--))))))))

;; Imperative lazy foreach (only allows a single variable, not multiple like
;; Racket's)
(define-macro (for-lazy var . rest)
  (let ([name      (car  var)]
        [list-expr (cadr var)])
    `(let ([--list-- ,list-expr])
       (let --loop-- ([--list-- --list--])
         (if (null? --list--)
             (void)
             (let ([,name (car --list--)])
               ,@rest
               (--loop-- (cdr-lazy --list--))))))))

(define-macro (list* first . rest)
  (if (null? rest)
      first
      `(cons ,first (list* ,@rest))))
