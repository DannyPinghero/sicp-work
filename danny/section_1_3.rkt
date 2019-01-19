#!/usr/bin/racket
#lang racket
(require racket/include)
(include "gcd.rkt")

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))

(define (cube x) (* x x x))

;  1.29
(define (simpson f a b n)
	(define h (/ (- b a) n))
	(define (add-h x) (+ x h h))
	(* (/ h 3) (+ 
		(f a) 
		(f b) 
		(* 4 (sum f (+ h a) add-h (- b h))) 
		(* 2 (sum f (+ h h a) add-h (- b h h)))
	))
)

; n = 100-> 1/4
; n = 1000 -> 1/4

; a = 0
; b = 1
; n = 4
; h = .25

; h/3 * (f(0) + 4*f(.25) + 2f(.5) + 4f(.75) + f(1))
; h/3 * (f(0) + f(1) + 
; 	4* something1 + 
; 	2*something2)
; something1 = sum f, .25, +.5, .75 => f(.25) + sum f, .75, +.5, .75 => f(.25) + f(.75) + 0
; something2 = sum f, .5, +.5, .5 => f(.5) + sum f, 1. +.5, .5 => f(.5) + 0

; 1.30
(define (sum-2 term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))))
	(iter (term a) 0))

; 1.31
(define (product f a next b)
	(if (> a b)
		1
		(* (f a) (product f (next a) next b))))

(define (product-2 f a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (f a)))))
	(iter a 1))
(define (inc x) (+ 1 x))
(define (identity x) x)
(define (fact-1 n)
	(product identity 1 inc n)
 )
(define (fact-2 n)
	(product-2 identity 1 inc n))

(define (pi-prod-1 b)
	(define (next x) (+ 2 x))
	(/ (* 8 (square (product identity 4 next b))) (* 1.0 b (square (product identity 3 next b)))))
	
;  * 8 for initial 2 and to get the 4 on the right hand side.  / b because the highest term appears only once

(define (acc-1 combiner initial-value term a next b)
	(if (> a b)
	initial-value
	(combiner (term a) (acc-1 combiner initial-value term (next a) next b))))

(define (acc-2 combiner initial-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (term a)))))
	(iter a initial-value))
(define (mult a b) (* a b))
(define (add a b) (+ a b))
(define (product-3 f a next b)
	(acc-1 mult 1 f a next b))
(define (product-4 f a next b)
	(acc-2 mult 1 f a next b))
(define (sum-3 f a next b)
	(acc-1 add 0 f a next b))
(define (sum-4 f a next b)
	(acc-2 add 0 f a next b))

;  1.33
(define (filtered-accumulate combiner initial-value term a next b filter)
	(cond 
		((> a b) initial-value)
		((filter a) (combiner (term a) (filtered-accumulate combiner initial-value term (next a) next b filter)))
		(else (combiner initial-value (filtered-accumulate combiner initial-value term (next a) next b filter)))))

;  sum squares primes
(define (sum-squares-primes a b)
	(filtered-accumulate add 0 square a inc b prime?))

(define (product-relative-primes n)
	(define (gcd-is-one x) (= 1 (gcd x n)))
	(filtered-accumulate mult 1 identity 2 inc n gcd-is-one))


; 1.34
; (f f) => (f 2) => (2 2) and this is an error because 2 is not a function

; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(display guess)
		(newline)
		(let ((next (f guess)))
			(if (close-enough? guess next)
			next
			(try next))))
	(try first-guess))
; x = 1 + 1/x => x - (1/x) = 1 => x^2 - 1 = x => x^2 - x - 1 = 0 =>
; (1 +/- sqrt(5)) / 2, phi is the positive root
(define (get-phi)
	(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.6))
; 1.36
(define (average x y) (/ (+ x y) 2))
(define (that-log-one)
	(fixed-point (lambda (x) (/ (log 1000) (log x))) 2))
(define (that-log-one-average)
	(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))
;  wayyyy fewer hops

; 1.38
; N1 / (D1 + (N2 / (D2 + ...)))


(define (cont-frac num denom k)
	(define (inner i)
		(if (< i k)
			(/ (num i) (+ (denom i) (inner (+ 1 i))))
			(/ (num i) (denom i))))
	(inner 1))


(define (get-phi-2 n)
	(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) n)))
;  4 decimal places is 1.6180[339887].  takes 12 terms to get 1.61805...

;  1.38
;  0 1 2 3 4 5 6 7 8 9  10 11
;  1 2 3 4 5 6 7 8 9 10 11 12
;  1 2 1 1 4 1 1 6 1  1  8  1 

(define (e-denom one two three)
	(cond
		((and (= 0 one) (= 0 two) (= 0 three)) 1)
		((and (= 0 one) (= 0 two) (= 1 three)) 2)
		((and (= 0 one) (= 1 two) (= 2 three) 1))
		((and (= 1 one) (= 1 three)) 1)
		((and (= 1 two) (= 1 three)) (+ 2 one))
		((and (= 1 one) (= 1 two)) 1)))

;  hm but i only get one number


;  1.39
(define (cont-tan x n)
	(cont-frac (lambda (i) (if (= i 1) x (* -1 x x))) (lambda (i) (- (* 2 i) 1)) n))
