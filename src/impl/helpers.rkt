#lang racket

(require (prefix-in b: "base.rkt"))

(provide pull
         take)

(define ((once g) s/c)
  (let loop ([$ (g s/c)])
    (cond
      [(null? $) '()]
      [(promise? $) (delay/name (loop (force $)))]
      [else (list (car $))])))

(define (pull $)
  (if (promise? $) (pull (force $)) $))

(define (take n $)
  (cond 
    [(null? $) '()]
    [(and n (zero? (- n 1))) (list (car $))]
    [else (cons (car $)
                (take (and n (- n 1))
                      (pull (cdr $))))]))

