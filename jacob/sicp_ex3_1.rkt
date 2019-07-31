(define (make-accumulator sum)
  (lambda (addend)
    (begin (set! sum (+ sum addend))
           sum)))


(define A (make-accumulator 5))

(A 10)

(A 5)