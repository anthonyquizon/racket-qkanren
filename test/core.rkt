#lang racket

(require "../src/core.rkt"
         rackunit)

(check-equal? 
  (run 1 [q]
    (== q 5))
  '(5))

;(check-equal? 
  ;(run* [q]
    ;(conde 
      ;[(== q 5)]
      ;[(== q 4)]))
  ;'(5 4))

