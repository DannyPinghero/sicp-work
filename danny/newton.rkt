#!/usr/bin/racket
#lang racket
; (require racket/include)
; (include "section_1_3.rkt")
(define (square x) (* x x))
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
			next
			(try next))))
	(try first-guess))


(define dx 0.00001)
(define (deriv g)
	(lambda (x) 
		(/ (- (g (+ x dx))(g x))
			dx)))
(define (newton-transform g)
	(lambda (x) 
		(- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
	(fixed-point (newton-transform g) guess))

(define (sqrt-newt x)
	(newtons-method (lambda (y) (- (square y) x))
		1.0))



; 1.40
(define (cubic a b c)
	(lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
(define (cubic-root a b c)
	(newtons-method (cubic a b c) 1))

; 1.41
(define (double func)
	(lambda (x) 
		(func (func x))))

(define (inc x)
	(+ x 1))
; (((double (double double)) inc) 5) => 21, or 16 incs

; 1.42
(define (compose f g)
	(lambda (x)
		(f (g x))))

; 1.43
(define (repeated f n)
	(define (inner f n i acc)
		(if (= n i) (f acc) (inner f n (+ 1 i) (f acc))))
	(lambda (x)
		(inner f n 1 x)))

; 1.44
(define (smooth f)
	(lambda (x)
		(/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)))

(define (n-fold-smooth f n)
	(lambda (x)
		(((repeated smooth n) f) x)))

; 1.45
(define (fourth-root x)
	(fixed-point (lambda (y) (/ x (square (square y)))) 1))

