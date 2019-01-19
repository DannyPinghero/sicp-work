#!/usr/bin/racket
#lang racket
(require racket/include)
(include "gcd.rkt")
;  section 2.1

; 2.1 (make it normalize the sign)
(define (make-rat n d)
	(let (
			(g (abs (gcd n d)))
			(norm-n (if (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0))) (abs n) (* -1 (abs n))))
			(norm-d (abs d))
		)
		(cons (/ norm-n g) (/ norm-d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
				 (* (numer y) (denom x)))
				(* (denom x) (denom y))))
(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
				 (* (numer y) (denom x)))
				(* (denom x) (denom y))))
(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
				(* (denom x) (denom y))))
(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
				(* (denom x) (numer y))))
(define (equal-rat? x y)
	(= (* (numer x) (denom y))
		(* (numer y) (denom x))))

; 2.2
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
)
(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))
(define (print-segment line)
	(newline)
	(print-point (start-segment line))
	(display "->")
	(print-point (end-segment line)))
(define (midpoint-segment line)
	(make-point 
		(/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
		(/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)
	)
)

; 2.3
; impl 1 - store top left and bottom right points
(define (make-rect top-left-point bottom-right-point)
	(cons top-left-point bottom-right-point)
)
(define (get-top-left rect) (car rect))
(define (get-bottom-right rect) (cdr rect))
(define (get-top-right rect) (make-point (x-point (get-bottom-right rect)) (y-point (get-top-left rect))))
(define (get-bottom-left rect) (make-point (x-point (get-top-left rect)) (y-point (get-bottom-right rect))))
(define (get-top-line rect) (make-segment (get-top-left rect) (get-top-right rect)))
(define (get-left-line rect) (make-segment (get-top-left rect) (get-bottom-left rect)))
(define (print-rect rect)
	(print-point (get-top-left rect))
	(print-point (get-top-right rect))
	(print-point (get-bottom-right rect))
	(print-point (get-bottom-left rect))
)

(define (get-length line)
	(sqrt (+ 
		(square (- (x-point (end-segment line)) (x-point (start-segment line)))) 
		(square (- (y-point (end-segment line)) (y-point (start-segment line))))
	)))
(define (get-area rect)
	(* (get-length (get-top-line rect)) (get-length (get-left-line rect))))
(define (get-perimeter rect)
	(* 2 (+ (get-length (get-top-line rect)) (get-length (get-left-line rect)))))

; impl 2 - store top and bottom line segments
(define (make-rect-2 top-line bottom-line) (cons top-line bottom-line))
(define (get-top-line-2 rect) (car rect))
(define (get-left-line-2 rect) (make-segment (start-segment (car rect)) (start-segment (cdr rect))))
;  etc

; 2.4
(define (cons2 x y) (lambda (m) (m x y)))  ; this returns a function which takes a function and applies it to x and y
(define (car2 z) (z (lambda (p q) p))) ; this passes z a function which, given 2 things, returns the first

(define (cdr2 z) (z (lambda (p q) q)))
; (car2 (cons2 1 2)) -> (car2 (lambda (m) (m 1 2))) => ((lambda (m) (m 1 2)) (lambda (p q) (p))) => (lambda ( 1 2) -> 2)

; 2.5
; 2^a * 3^b
; this will work because 2 and 3 are primes.  All prime factorizations are unique, so no other pair a,b could ever return this product
(define (cons3 a b) (* (expt 2 a) (expt 3 b)))
; ex car3 (cons 2 3) => car3(2^2 * 3^ 3) = car3(108)
; inner(108 1) -> (inner 54 2) -> (inner 27 3) -> 2
(define (car3 m) ; how many factors of 2 are there
	(define (inner m n)
		(if (= 0 (remainder m 2)) (inner (/ m 2) (+ 1 n)) (- n 1)))
	(inner m 1))
(define (cdr3 m)
	(define (inner m n)
		(if (= 0 (remainder m 3)) (inner (/ m 3) (+ 1 n)) (- n 1)))
	(inner m 1))



; 2.6
; (define fzero (lambda (f) (lambda (x) x)))
; ; zero is a function that takes one argument and returns a function that takes one argument and returns it - it doesn't invoke the outer function
; (define (add-1 n)
; 	(lambda (f) (lambda (x) (f ((n f) x)))))
; ; add one takes a function and returns a function that calls that function an extra time?

; (add-1 zero) 
; ->
; (lambda (f) (lambda (x) (f ((zero f) x))))
; ; take a function f that returns a function that
; ; takes a function x that calls f on the result of calling (zero on f and then calling that on x)
; ->
; (lambda (f) (lambda (x) (f (lambda (x') x') x)))
; -> (lambda (f) (lambda (x) (f x)))

; ; so one: takes a function that returns a function that takes an argument and then applies the outer function to that argument _one_ time
; ; and 0 takes a function that retuns a function that takes an argument and applies the outer funcion to the argument 0 times
; ;  two would do it two times
; -> two = lambda f: lambda x: f(f(x))

; with a lot of help from
; http://www.billthelizard.com/2010/10/sicp-26-church-numerals.html

(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
			(p2 (* (lower-bound x) (upper-bound y)))
			(p3 (* (upper-bound x) (lower-bound y)))
			(p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4)
						(max p1 p2 p3 p4))))
; modified in 2.10
; (define (div-interval x y)
; 	(mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
; 2.7
(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))
; 2.8
(define (sub-interval x y)
	;  consider (1, 3) - (2, 4), which should be (-3, 1)
	; and (2, 4) - (1, 3), which should be (-1, 3)
	;  so its always (1st one small - 2nd one big, 1st one big, 2nd one smalle
	(make-interval (- (lower-bound x) (upper-bound y)) (- (upper-bound x) (lower-bound y))))
; 2.9
; it is known that uncertainty of addition is the sum of uncertainties, whereas multiplication its the sum of the relative uncertainties
; ex (1+/-1) + (2+/-1)
; => (1,5) = 3+/-2 <=> (3 +/- 2)  2 == 1 + 1

; (2 +/- 1) * (3 +/- 1) = 
; (1, 3) * (2, 4) = (2, 12) = (7 +/5)
; (2 +/- 1) * (4 +/- 1)
; (1, 3) * (3, 5) = (3, 15) = (9 +/- 6)
; all intervals had widths of 1, but the answers have different widths

; 2.10
(define (div-interval x y)
	(if 
		(and (< (lower-bound y) 0) (> (upper-bound y) 0))
		(display "Divisor must not span 0")
		(mul-interval
			x 
			(make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))
		)))
; 2.11
; im going to ignore that first commnet, Ben
(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))
(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))
; 2.12
(define (make-center-percent c p)
	(let ((width (* c p)))
		(make-center-width c width)))
(define (percent i)
	(let ((w (width i))
			(c (center i)))
		(/ w c)))
; 2.13
; They add

; 2.14
(define A (make-center-percent 10 .001))
(define B (make-center-percent 11 .001))
(define (par1 r1 r2)
	(div-interval (mul-interval r1 r2) (add-interval r1 r2)))
(define (par2 r1 r2)
	(let ((one (make-interval 1 1)))
		(div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))
; indeed (par1 A B) != (par2 A B)

; 2.15
; https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
; ¯\_(ツ)_/¯











