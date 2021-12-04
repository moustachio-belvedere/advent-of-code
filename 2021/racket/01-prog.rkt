#!/bin/racket
#lang racket

;; part 1 - tail recursive and O(n)
(define (depth-comparator depths prev-depth acc)
  (if (null? depths)
      acc
      (depth-comparator (cdr depths)
                        (car depths) 
                        (if (> (car depths) prev-depth)
                            (+ acc 1)
                            acc))))

;; part 2 (two sepearate implementations provided)

;; utility function
(define (3-add lin)
  (+ (car lin)
     (cadr lin)
     (caddr lin)))

;; tail recursive / recursive procedure, iterative process
;; assumes list length >= 3, O(n)
(define (moving-average-tail lin init)
  (if (null? (cddr lin))
      init
      (moving-average-tail (cdr lin)
                           (append init
                                   (list (3-add lin))))))

;; regular recursive / recursive procedure, recursive process
(define (moving-average-recursive lin)
  (if (null? (cddr lin))
      '()
      (cons (3-add lin) (moving-average-recursive (cdr lin)))))

(module+ main
  (define input (map string->number
                     (file->lines "01-realinput-p1.txt")))

  (depth-comparator input +inf.0 0)
  (depth-comparator (moving-average-tail input '())
                    +inf.0
                    0)
  (depth-comparator (moving-average-recursive input)
                    +inf.0
                    0)
)

;; test
(module+ test
  (require rackunit)
  (define testinput (map string->number
                         (file->lines "01-testinput.txt")))

  (define (p1 testinput)
    (depth-comparator testinput +inf.0 0))

  (define (p2-tail-recursive lin)
    (depth-comparator (moving-average-tail lin '())
                      +inf.0
                      0))

  (define (p2-recursive lin)
    (depth-comparator (moving-average-recursive lin)
                      +inf.0
                      0))

  (check-equal? (p1 testinput) 7 "Day 1, part 1")
  (check-equal? (p2-tail-recursive testinput) 5 "Day 1, part 2")
  (check-equal? (p2-recursive testinput) 5 "Day 1, part 2")
)
