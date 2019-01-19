#!/usr/bin/racket
; #lang racket/base


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


; 1.20
; gcd(206, 40) =>
; remainder(206, 40), gcd(40, 6) =>
; remainder(40, 6), gcd(6, 4) =>
; remainder(6, 4), gcd(4, 2) =>
; remainder(4, 2), gcd(2, 0) =>
; 2

; Theyre not different?  because b always needs to be evaluated, which is the remainder from the last one?

(define (square n) (* n n))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
  		((divides? test-divisor n) test-divisor)
    	(else (find-divisor n (next test-divisor)))))
    	; (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp) (remainder (square (expmod base (/ exp 2) m))
			m))
		(else (remainder (* base (expmod base (- exp 1) m))
			m))))
(define (fermat-test n)
	(define (try-it a)
		; (= (expmod-2 a n n) a))
		(= (expmod-2 a n n) a))
	(try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
	(cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))





; 1.21
;  199 - prime
;  1999 - prime
;  7 - not prime

; 1.22
(define (timed-prime-test n)
	(newline)
	(display n)
	(newline)
	(start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
	(if (fast-prime? n 10)
	; (if (prime? n)
		(report-prime (- (current-inexact-milliseconds) start-time))
		#f))
(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
	(newline))

(define (search-for-primes start num-primes)
	(cond
		((= 0 num-primes) #f)
		((even? start) (search-for-primes (+ 1 start) num-primes))
		((timed-prime-test start) (search-for-primes (+ 2 start) (- num-primes 1)))
		(else (search-for-primes (+ 2 start) num-primes))))


;  > 1000
;  1009 
;  1013
;  1019
;  all about .005

; >10000
; 10007
; 10009
; 10037
; between .008 and .02

; >100000
; 100003
; 100019
; 100043
; between .014 and .04

; >1000000
; 1000003
; 1000033
; 1000037
; between .04 and .06

; .005 * sqrt 10 is .015, so yea not bad
; .015 * sqrt 10 is .04, also not bad


(define (next n)
	(if (= n 2) 3 (+ n 2)))

; 1.23
; > 1000, ~.003
; > 10000 ~.007 
; > 100000~.02 - .03
; > 1000000 ~.03

; Not quite 2x, I think because we're not looking very far, skipping doesn't have that much time to work in 37 numbers

;  1.24
; 1000 .02
; 10000 .014-.02
; 100000 .016 - .02
; 1000000 .03
;  not really log?

(define (fast-exp b n)
	(cond 
		((= n 0) 1)
  		((even? n) (square (fast-exp b (/ n 2))))
    	(else (* b (fast-exp b (- n 1))))
	)
)

; 1.25
; no, this slows it down immensely.  I think because our expmod takes keeps it small, while this gets a big number and then remainders is
(define (expmod-2 base exp m) (remainder (fast-exp base exp) m))

; 1.26
; using square only evaluates the expmod 1 time, rather than the twice in doofus' code

; 1.27
;  the first carmichael number is 561
;  then 1105 1729, etc
(define (test-carm num)
	(define (test-carm-inner n a)
		(cond 
			((= a 0) #t)
			(else (if (= (remainder a n) (expmod a n n)) (test-carm-inner n (- a 1)) #f))
		))
	(test-carm-inner num (- num 1)))

;  1.28
;  hmm



(provide (all-defined-out))
