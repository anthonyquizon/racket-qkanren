#lang racket

(require "../src/core.rkt"
         rackunit)

(check-equal? 
  (run 1 [q]
    (== q 5))
  `((#hash((=/= . ()) 
           (== . ((,(var 0) . 5)))) . 1)))

(check-equal? 
  (run 1 [q]
    (fresh [a] 
      (== a q)))
  `((#hash((=/= . ())
           (== . ((,(var 1) . ,(var 0))))) . 2)))


(check-equal? 
  (run 2 [q]
    (conde 
      [(== q 5)]
      [(== q 4)]))
  `((#hash((=/= . ())
           (== . ((,(var 0) . 5)))) . 1)
    (#hash((=/= . ())
           (== . ((,(var 0) . 4)))) . 1)))

(check-equal? 
  (run 1 [q]
    (=/= q 5))
  `((#hash((=/= . ((,(var 0) . 5)))
           (== . ())) . 1)))
