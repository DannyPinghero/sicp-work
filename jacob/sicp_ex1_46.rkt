(define (iterative-improve good-enough? improve)
  (define (internal-improve guess)
    (if (good-enough? guess)
        guess
        (internal-improve (improve guess))))
  internal-improve)

(define (square x) (* x x))

;Just had the commented out bits a reference
;(define (improve guess x)
;  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;(define (sqrt x)
;  (sqrt-iter 1.0 x))


(define (sqrt-good-e x)
  (lambda (guess)
    (< (abs (- (square guess) x)) 0.001)))

(define (sqrt-improve x)
  (lambda (guess)
    (average guess (/ x guess))))

(define (new-sqrt x)
  ((iterative-improve (sqrt-good-e x) (sqrt-improve x)) 1.0))


(new-sqrt 16)


;(define tolerance 0.00001)
;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))


(define (fixed-good-e func)
  (lambda (guess)
    (< (abs (- (func guess) guess)) 0.00001)))

(define (fixed-improve func)
  (lambda (guess)
    (func guess)))

(define (new-fixed func)
  ((iterative-improve (fixed-good-e func) (fixed-improve func)) 1.0))

(new-fixed cos)