(define (compose f g)
  (lambda (x)
    (f (g x))))
             
(define (repeated func n)
  (if (= 1 n) 
      func
      (compose func (repeated func (- n 1)))))

(define (inc i) (+ i 1))

(define (square x) (* x x))

((repeated inc 5) 3)

((repeated square 2) 5)
