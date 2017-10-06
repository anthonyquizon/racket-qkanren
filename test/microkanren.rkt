#lang racket

(require "../src/microkanren.rkt"
         rackunit)

(check-equal? 
  ((call/fresh 
     (lambda [q] (== q 5))) empty-s) 
  '((((#(0) . 5)) . 1)))

(check-equal? 
  ((call/fresh 
     (lambda [q] 
       (conj (== q 5) (== q 6)))) empty-s) 
  '())

(check-equal? 
  ((call/fresh 
     (lambda [q] 
       (disj (== q 5) (== q 6)))) empty-s) 
  '((((#(0) . 5)) . 1) (((#(0) . 6)) . 1)))

(check-equal? 
  ((call/fresh 
     (lambda [q] 
       (conj 
         (call/fresh  
           (lambda [r]
             (disj (== q 5) (== q r))))
         (== q 5)))) empty-s) 
  '((((#(0) . 5)) . 2)
    (((#(1) . 5) (#(0) . #(1))) . 2)))

