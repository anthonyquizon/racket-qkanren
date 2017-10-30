#lang racket

(require (for-syntax syntax/parse)
         (prefix-in b: "base.rkt"))

(provide
  S0
  ==
  =/=
  define-relation
  )

(define (ext-S S key terms) 
  (hash-update S key ((curry cons) (apply list* terms))))

(define (((make-constraint-goal-constructor key) . terms) S/c)
  (let ([S (ext-S (car S/c) key terms)])
    (if (invalid? S) '() (list `(,S . ,(cdr S/c))))))

(define-syntax-rule (make-invalid? (cid ...) p ...)
  (lambda [S] 
    (let ([cid (hash-ref S 'cid)] ...)
      (cond 
        [(valid-== (hash-ref S '==)) 
         => (lambda [s] (or (p s) ...))]
        [else #t]))))

(define-syntax-rule (define-relation (rid . args) g)
  (define ((rid . args) S/c) (delay/name (g S/c))))

(define (valid-== ==)
  (foldr 
    (lambda [pr s] (and s (b:unify (car pr) (cdr pr) s))) '() ==))

(define-syntax (make-constraint-system stx)
  (syntax-parse stx
    [(_ [cid:id ...] p ...)
     (with-syntax ([invalid? (syntax-local-introduce #'invalid?)]
                   [S0 (syntax-local-introduce #'S0)]
                   [== (syntax-local-introduce #'==)])
       #'(begin 
           (define invalid? (make-invalid? (cid ...) p ...))
           (define S0 (make-immutable-hash '((==) (cid) ...)))
           (define == (make-constraint-goal-constructor '==))
           (define cid (make-constraint-goal-constructor 'cid))
           ...))]))


(define (same-s? u v s) (equal? (b:unify u v s) s))

(make-constraint-system 
  (=/=)
  (lambda [s] 
    (ormap (lambda [pr] 
             (same-s? (car pr) (cdr pr) s)) =/=))) 

