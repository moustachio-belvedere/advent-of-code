#lang racket

(define (depth-comparator depths prev-depth acc)
  (if (null? depths)
      acc
      (depth-comparator (cdr depths)
                        (car depths) 
                        (if (> (car depths) prev-depth)
                            (+ acc 1)
                            acc))))

(depth-comparator (map string->number
                       (file->lines "01-testinput.txt"))
                  +inf.0
                  0)
