#!/bin/racket
#lang racket

(define (position-cal plan depth extent)
  (if (null? plan)
      (list depth extent)
      (let ((instruc (car plan)))
        (let ((dir (car instruc))
              (mag (cadr instruc)))
          (cond ((string=? dir "forward")
                 (position-cal (cdr plan)
                               depth
                               (+ extent mag)))
                ((string=? dir "up")
                 (position-cal (cdr plan)
                               (- depth mag)
                               extent))
                ((string=? dir "down")
                 (position-cal (cdr plan)
                               (+ depth mag)
                               extent)))))))

(define (position-cal-p2 plan depth extent aim)
  (if (null? plan)
      (list depth extent)
      (let ((instruc (car plan)))
        (let ((dir (car instruc))
              (mag (cadr instruc)))
          (cond ((string=? dir "forward")
                 (position-cal-p2 (cdr plan)
                                  (+ depth (* aim mag))
                                  (+ extent mag)
                                  aim))
                ((string=? dir "up")
                 (position-cal-p2 (cdr plan)
                                  depth
                                  extent
                                  (- aim mag)))
                ((string=? dir "down")
                 (position-cal-p2 (cdr plan)
                                  depth 
                                  extent
                                  (+ aim mag))))))))

(define (input-wrangle f)
  (let ((lines (file->lines f)))
    (map (λ (vec)
            (let ((split-vec (string-split vec)))
              (list (car split-vec)
                    (string->number (cadr split-vec)))))
         lines)))

(module+ main
  (define travelled (position-cal (input-wrangle "02-realinput.txt") 0 0))
  (* (car travelled) (cadr travelled))

  (define travelled-p2 (position-cal-p2 (input-wrangle "02-realinput.txt") 0 0 0))
  (* (car travelled-p2) (cadr travelled-p2))

)

;; test
(module+ test
  (require rackunit)

  (define travelled (position-cal (input-wrangle "02-testinput.txt") 0 0))
  (check-equal? (* (car travelled) (cadr travelled)) 150 "Day 2, part 1")
)
