(define (make-interval a b) (cons a b))

(define (upper-bound inter) (cdr inter))
(define (lower-bound inter) (car inter))