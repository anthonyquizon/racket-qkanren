#lang racket

(require (prefix-in b: "base.rkt")
         (prefix-in h: "helpers.rkt")
         (prefix-in c: "constraints.rkt"))

(require (for-syntax (prefix-in b: "base.rkt"))
         (for-syntax (prefix-in h: "helpers.rkt"))
         (for-syntax (prefix-in c: "constraints.rkt")))

(provide 
  conde
  fresh
  run)

(define-syntax Zzz
  (syntax-rules []
    [(_ g) (lambda (s/c) 
             (lambda [] (g s/c)))]))

(define-syntax conj+
  (syntax-rules []
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (b:conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules []
    [(_ g) (Zzz g)]
    [(_ g0 g ...) (b:disj (Zzz g0) (disj+ g ...))])) 

(define-syntax conde
  (syntax-rules []
    [(_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...)]))

(define-syntax fresh
  (syntax-rules []
    [(_ [] g0 g ...) (conj+ g0 g ...)]
    [(_ [x0 x ...] g0 g ...)
     (b:call/fresh (lambda [x0] (fresh [x ...] g0 g ...)))]))

(define-syntax-rule (run n/b (q) g g* ...)
  (call/initial-state n/b (fresh (q) g g* ...)))

(define (call/initial-state n g) 
  (h:take n (h:pull (g `(,c:S0 . 0)))))
