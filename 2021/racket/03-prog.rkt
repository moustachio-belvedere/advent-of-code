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

(define (most-common-bit bn i)
  (ocb bn i >))

(define (most-common-bit bn i)
  (ocb bn i <))

;; * commont bit at all indices
(define (*-common-bits bn op)
  (define (biterator bn acc i len)
    (if (>= i len)
        acc
        (biterator bn
                   (append acc
                           (list (ocb bn i op)))
                   (+ i 1)
                   len)))

  (biterator bn '() 0 (length (car bn))))

(define (most-common-bits bn)
  (*-common-bits bn >))

(define (least-common-bits bn)
  (*-common-bits bn <))

(define (binarylist->decimal bin)
  (define (bitter bin i sum)
    (let ((bit (car bin)))
    (if (= i 0)
        (+ sum bit)
        (bitter (cdr bin)
                (- i 1)
                (if (zero? bit)
                    sum
                    (+ sum (expt 2 i)))))))

  (bitter bin (- (length bin) 1) 0))
  
(define (input-wrangle f)
  (let ((lines (file->lines f)))
    (tree-map (λ (v)
                 (- (char->integer v) 48))
              (map string->list lines))))

(define (power lines)
  (* (binarylist->decimal (most-common-bits lines))
     (binarylist->decimal (least-common-bits lines))))

(module+ main
  (define in (input-wrangle "03-realinput.txt"))


)
