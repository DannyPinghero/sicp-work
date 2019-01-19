#!/usr/bin/racket
#lang racket/base

(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x) x)
	)
)

(define (improve guess x)
	(average guess (/ x guess))
)

(define (average x y)
	(/ (+ x y) 2)
)

(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001)
)

(define (square x) (* x x))

(define (sqrt x)
	(sqrt-iter 1.0 x)
)



;; 1.6

;; it locks up.  I thinkkkkk its because now the 2nd argument has to be evaluated, because it's being passed to a function.  whereas the if is a special form so it doesn't evaluate it unless it needs to

(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
		(else else-clause))
)

(define (sqrt-iter-2 guess x)
	(new-if 
		(good-enough? guess x)
		guess
		(sqrt-iter-2 (improve guess x) x)
	)
)

;; 1.7
; If you try to find the sqrt of say .0001, anything less than .001 will work out.  Once you get large enough, I guess the distance between 2 floats is going to be greater than the tolerance?  Or something?

(define (good-enough-2? cur-guess last-guess x)
	(< (/ (abs (- cur-guess last-guess)) x) .001)
)

(define (sqrt-iter-3 cur-guess last-guess x)
	(if (good-enough-2? cur-guess last-guess x)
		cur-guess
		(sqrt-iter-3 (improve cur-guess x) cur-guess x)
	)
)

(define (sqrt-2 x)
	(sqrt-iter-3 1.0 x x)
)
;; it is better!

;; 1.8
(define (improve-cube guess x)
	(/ (+ (/ x (square guess) (* 2 guess))) 3)
)

(define (cube-iter cur-guess last-guess x)
	(if (good-enough-2? cur-guess last-guess x)
		cur-guess
		(cube-iter (improve cur-guess x) cur-guess x)
	)
)

(define (cube-root x)
	(cube-iter 1.0 x x)
	;  gives quite a bad answer - double check
)

