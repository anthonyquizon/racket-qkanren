#lang racket

(require "../src/core.rkt"
         rackunit)

(check-equal? 
  ((call/fresh 
     (lambda [q] (== q 5))) empty-state) 
  '((((#(0) . 5)) . 1)))

(check-equal? 
  ((call/fresh 
     (lambda [q] 
       (conj (== q 5) (== q 6)))) empty-state) 
  '())


(check-equal? 
  ((call/fresh 
     (lambda [q] 
       (disj (== q 5) (== q 6)))) empty-state) 
  '((((#(0) . 5)) . 1) (((#(0) . 6)) . 1)))

(check-equal? 
  ((call/fresh 
     (lambda [q] 
       (conj 
         (call/fresh  
           (lambda [r]
             (disj (== q 5) (== q r))))
         (== q 5)))) empty-state) 
  '((((#(0) . 5)) . 2)
    (((#(1) . 5) (#(0) . #(1))) . 2)))

(define (sixes x)
  (disj (== x 6) (lambda [s/c]  (lambda [] ((sixes x) s/c)))))

(define (fives x)
  (disj (== x 5) (lambda [s/c]  (lambda [] ((fives x) s/c)))))

(check-equal? 
  (let* [($ ((call/fresh (lambda [x] (disj (fives x) (sixes x)))) empty-state))
         ($^ ((cdr $)))
         ($^^ ((cdr $^)))
         (s (car $))
         (s^ (car $^))
         (s^^ (car $^^))]
    `(,s ,s^ ,s^^))
  '((((#(0) . 5)) . 1) 
    (((#(0) . 6)) . 1)
    (((#(0) . 5)) . 1)))

