#lang racket

(require (prefix-in c: "./core.rkt")
         (prefix-in h: "./helpers.rkt"))

(require (for-syntax (prefix-in c: "./core.rkt"))
         (for-syntax (prefix-in h: "./helpers.rkt")))

(provide Zzz
         conj+
         disj+
         conde
         fresh
         run
         run*)

(define-syntax Zzz
  (syntax-rules []
    [(_ g) (lambda (s/c) 
             (lambda [] (g s/c)))]))

(define-syntax conj+
  (syntax-rules []
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (c:conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules []
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (c:disj (Zzz g0) (disj+ g ...))])) 

(define-syntax conde
  (syntax-rules []
    [(_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...)]))

(define-syntax fresh
  (syntax-rules []
    [(_ [] g0 g ...) (conj+ g0 g ...)]
    [(_ [x0 x ...] g0 g ...)
     (c:call/fresh (lambda [x0] (fresh [x ...] g0 g ...)))]))

(define-syntax run
  (syntax-rules []
    [(_ n [x ...] g0 g ...)
     (h:reify (h:take n (c:call/empty-state 
                          (fresh [x ...] g0 g ...))))]))

(define-syntax run*
  (syntax-rules []
    [(_ [x ...] g0 g ...)
     (h:reify (h:take-all (c:call/empty-state
                            (fresh [x ...] g0 g ...))))]))


