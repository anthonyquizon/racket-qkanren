#lang racket

(provide empty-s == call/fresh conj disj call/empty-s) 

(define (assp proc alist)
  (let loop ((alist alist))
    (if (null? alist)
      #f
      (let ((p (car alist)))
        (if (proc (car p))
          p
          (loop (cdr alist)))))))

(define empty-s '(() . 0))
(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(define (walk u s)
  (let [(pr (and (var? u) (assp (lambda [v] (var=? u v)) s)))]
    (if pr (walk (cdr pr) s) u)))

(define (occurs-check x v s)
  (let [(v (walk v s))]
    (cond
      [(var? v) (var=? v x)]
      [else (and (pair? v) (or (occurs-check x (car v) s)
                               (occurs-check x (cdr v) s)))])))

(define (ext-s x v s) 
  (if (occurs-check x v s) #f `((,x . ,v) . ,s)))

(define (== u v)
  (lambda [s/c]
    (let [(s (unify u v (car s/c)))]
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let [(u (walk u s))
        (v (walk v s))]
    (cond 
      [(and (var? u) (var? v) (var=? u v)) s] 
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let [(s (unify (car u) (car v) s))]
         (and s (unify (cdr u) (cdr v) s)))]
      [else (and (eqv? u v) s)])))

(define (call/empty-s g) (g empty-s))

(define (call/fresh f)
  (lambda [s/c]
    (let [(c (cdr s/c))]
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) 
  (lambda [s/c]
    (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2) 
  (lambda [s/c]
    (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
    [(null? $1) $2]
    [(procedure? $1) (lambda [] (mplus $2 ($1)))]
    [else (cons (car $1) (mplus (cdr $1) $2))]))

(define (bind $ g)
  (cond
    [(null? $) mzero]
    [(procedure? $) (lambda [] (bind ($) g))]
    [else (mplus (g (car $)) (bind (cdr $) g))]))

