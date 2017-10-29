#lang racket

(require racket/struct)

(provide 
  call/fresh 
  conj 
  disj 
  ifte
  var 
  var? 
  unify
  walk) 

(struct var (n)
  #:methods 
  gen:custom-write
  [(define write-proc
     (make-constructor-style-printer 
       (lambda [_v] 'var)
       (lambda [v] `(,(var-n v)))))]
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (equal?-recur (var-n a) (var-n b)))
   (define (hash-proc a hash-recur)
     (hash-recur (* 2341239121 (var-n a))))
   (define (hash2-proc a hash2-recur)
     (hash2-recur (var-n a)))])

(define ((call/fresh f) S/c)
  (let [(S (car S/c))  
        (c (cdr S/c))]
    ((f (var c)) `(,S . ,(+ c 1)))))

(define (occurs? x v s)
  (let [(v (walk v s))]
    (cond
      [(var? v) (equal? v x)]
      [(pair? v) (or (occurs? x (car v) s)
                    (occurs? x (cdr v) s))]
      [else #f])))

(define (ext-s x v s) 
  (if (occurs? x v s) #f `((,x . ,v) . ,s)))

(define (walk u s)
  (let ([pr (assv u s)])
    (if pr (walk (cdr pr) s) u)))

(define (unify u v s)
  (let [(u (walk u s))
        (v (walk v s))]
    (cond 
      [(equal? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let [(s (unify (car u) (car v) s))]
         (and s (unify (cdr u) (cdr v) s)))]
      [else #f])))

(define ((disj g1 g2) S/c) 
  ($append (g1 S/c) (g2 S/c)))

(define ((conj g1 g2) S/c) 
  ($append-map g2 (g1 S/c)))

(define ($append $1 $2)
  (cond
    [(null? $1) $2]
    [(promise? $1) 
     (delay/name ($append $2 (force $1)))]
    [else (cons (car $1) 
                ($append (cdr $1) $2))]))

(define ($append-map g $)
  (cond
    [(null? $) '()]
    [(promise? $) 
     (delay/name ($append-map g (force $)))]
    [else ($append (g (car $)) 
                   ($append-map g (cdr $)))]))

(define ((ifte g1 g2 g3) s/c)
  (let loop ([$ (g1 s/c)])
    (cond
      [(null? $) (g3 s/c)]
      [(promise? $) (delay/name (loop (force $)))]
      [else ($append-map $ g2)])))

