#!/bin/racket
#lang racket

(define (tree-map f lin)
  (map (λ (v)
          (if (pair? v)
              (tree-map f v)
              (f v))) lin))

;; cheeky recursion, essentially a 0-indexed getter function
;; for any list
(define (ith-bit bn i)
  (if (= i 0)
      (car bn)
      (ith-bit (cdr bn) (- i 1))))

(define (gen-bit-selector i)
  (λ (bn)
     (ith-bit bn i)))

(define (ocb bn i op)
  (let ((bitsum (apply + (map (gen-bit-selector i) bn))))
    (if (op (/ bitsum (length bn)) 0.5)
        1
        0)))

;; Most Common Bit at index i
(define (mcb bn i)
  (ocb bn i >))

;; Least Common Bit at index i
(define (lcb bn i)
  (ocb bn i <))

(define (bitsum lin acc)
  (if (null? lin)
      acc
  
(define (input-wrangle f)
  (let ((lines (file->lines f)))
    (tree-map (λ (v)
                 (- (char->integer v) 48))
              (map string->list lines))))

(module+ main
  (define in (input-wrangle "03-testinput.txt"))

  (mcb in 0)
  (lcb in 0)

)
