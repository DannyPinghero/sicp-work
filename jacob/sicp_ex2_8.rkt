(define (make-interval a b) (cons a b))

(define (upper-bound inter) (cdr inter))
(define (lower-bound inter) (car inter))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(sub-interval (make-interval 5 7) (make-interval 2 3))