(define (inc i) (+ i 1))

(define (double func)
  (lambda (x) 
    (func (func x))))

((double inc) 1) ;just making sure I didn't mess up

(((double (double double)) inc) 5)

