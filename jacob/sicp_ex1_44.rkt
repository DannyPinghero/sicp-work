(define dx 0.001)

(define (three_avg first second third)
  (/ (+ first second third) 3))


(define (smooth func)
  (lambda (x)
    (three_avg (func (- x dx))
               (func x)
               (func (+ x dx)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))
             
(define (repeated func n)
  (if (= 1 n) 
      func
      (compose func (repeated func (- n 1)))))

(define (n_fold_smooth n) ; Apparently this grows a lot as you increase n
  (repeated smooth n))

(define (square x) (* x x))


(square 22)
((smooth square) 22)
(((n_fold_smooth 10) square) 22)

