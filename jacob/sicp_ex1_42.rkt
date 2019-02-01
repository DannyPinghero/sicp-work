(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (inc i) (+ i 1))

(define (square x) (* x x))

((compose square inc) 6)