#lang racket

(require "../src/core.rkt"
         rackunit)

(check-equal? 
  (run 1 [q]
    (== q 5))
  '((#hasheqv((== . ((0 . 5)))) . 1)))

(check-equal? 
  (run 2 [q]
    (conde 
      [(== q 5)]
      [(== q 4)]))
  '((#hasheqv((== . ((0 . 5)))) . 1)
    (#hasheqv((== . ((0 . 4)))) . 1)))

