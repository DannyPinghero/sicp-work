#!/usr/bin/racket
#lang racket

; 2.77
First off, we also need to
(define (magnitude z)
	(apply-generic 'magnitude z))


calling it invokes apply-generic once, to dispatch on the concept of magnitude to the complex package
then the complex package invokes apply generic, dispatching on type (rect or polar)

; 2.78 just check pair? first, and if its not a pair, assume its a scheme number

; 2.79
(define (equ? x y) (apply-generic 'equ? x y))

; then in each of the packages we have to define it

; regs numbers
(put 'equ? 'scheme-number 'scheme-number (lambda (x y) (= x y)))
; rational
(put 'equ? 'rat 'rat (and (= (numer x) (numer y)) (= (denom x) (denom y))))
; similar for complex, but with real-part and imag-part


; 2.80
; it would be neat if we could just
; (define (=zero? x) (equ? x 0))
; but we can't because type(0) == scheme number and type(x) == ??
; and we've only defined equ? for comparing same num to same num (theres no upcasting defined yet)
; so we have to do it painfully

(define (=zero? x) (apply-generic '=zero? x))
(put '=zero 'scheme-number (lambda (x) (equ? (make-scheme-number 0) x)))
(put '=zero 'rat (lambda (x) (= 0 (numer x))))
(put '=zero 'complex (lambda (x) (equ? (make-from-real-imag 0 0) x)))

; 2.81

; a) It'll infinite loop - sees that there isnt an operation for these types, then sees a coercion from one to the other and attempts to coerce to itself, calls again, etc
; b) No, it was fine - if theres a procedure the (if proc) works fine
; c) add a branch to the cond that if the types are equal (and there isnt an operation defined), error

; 2.82

; eh yea do what it says, try coercing each thing to each one in turn
; imagine the case where we have a function int -> complex -> int, and I call it with a rational number
; f(1, 2/3)
; it will say that f is not defined for this type combo, true
; Then it will first try to make both args ints, which isn't possible and so will fail, ok
; Then it will try to make both args rational, which is possible, but still not find an operation with this type sig

; 2.83
; define an operation raise
; for int its something like
(define (raise n)
	(make-rational n 1))
; etc

; 2.84
; so we basically need a way to say, which of these two types is above the other
; if they were one level separated - which of them has a raise to the other
; But if they arent, say we have int and complex, well

; we _could_ just keep trying, I guess?  see if the first one can be raised to the second, or the second to the first.  If not,
; try raise(raise(the first one)) and raise(raise(the second)) and so on

; For n arguments, ummm this mightttttttttt be a little time consuming, but it does mean that the types
; never need to know how many others there are and so on

; The internet suggests adding a (type-level) function that returns an integer.  This is attractive, as it just tells
; you right away which is higher, and the difference between them defines how many raises to do.
; But it kind of sucks if you want to add  new type in the middle?  Then either everything above
; it needs to be incremented, or you need to use floats (ie name that one 2.5), but then you lose the
; 'this is the number of raises'.  Well I guess you could ceil the difference?  No because you don't know,
; if you have a 1 and a 2.5, if theres also a 1.5 or whatever.

; So probably faster to decide which is higher, and then you just have to recursive raise until you hit
; what you want, which isn't bad

; 2.85
So the problem basically tells you exactly what to do
Im going to call it lower instead of project because project is a terrible name
; complex
(define (lower z) (real z))
; real
; this one is actually non trivial, because if the number is stored in a computer, its rationalizeable
; but with loss of precisions probably
; but I guess the equals check will kind of tie that up anyway
(define (lower r)
	(define (num-digits-decimal decimal)
		; normally youd take an int and count how many times you can divide by 10 til you get something less than 10
		; here were going to start with a decimal, and multiply by 10 til we have an integer
		(define (inner num n)
			(if (= (num - (floor num)) num)
				n
				(inner (* 10 num) (+ 1 n))))
		(inner decimal 0))
	(let (
		(int-part (- r (floor r)))
		(decimal-part (- r (- (r - floor r)))) ; reall should just do r - int-part but you cant use things in the let in the let head
		(let (
			(num-digits-decimal (get-digits decimal-part)))
			(make-rational (+ int-part (* decimal-part num-digits-decimal)) (exp 10 num-digits-decimal))))))

; rational
; either numer is a multiple of denom (ie 4/2), or denom is 1, or it just can't be lowered
(define (lower x)
	(define (integer? x)
		; probably can be a smarter test but ok
		; if there is no difference between you and your floor, you're an integer
		(= (- x (floor x)) 0))
	(cond
		(integer? (/ (numer x) (denom x)))
		(else (numer x)))) ; this is the path that will fail the equals check, unless denom is 1 in which case we got lucky


; then drop will just check if the lowered thing is equal to the original
(define (drop x)
	(if (equ? (lower x) x)
		(lower x)
		#f)) ; idk what exactly the caller wants, but probably not an error
; note that this equ? will implicitly have to raise the first argument

; 2.86
; yea like it says we just need sqrt and sine and cosine to work on any number type, instead of the
; built in scheme number like befre

; 2.87 -2.97
; yea they seem fine
