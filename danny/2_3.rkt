#!/usr/bin/racket
#lang racket

; 2.53
; '( a b c)
; '((george))
; '(y1 y2)
; '(y1 y2)
; #f
; #f
; '(red shoes blue socks)

; 2.54
(define (list-eq l1 l2)
	(cond 
		; if both are empty, theyre equal
		((and (null? l1) (null? l2)) #t)
		; but if only one is, theyre not
		((or (null? l1) (null? l2)) #f)
		((and (eq? (car l1) (car l2)) (list-eq (cdr l1) (cdr l2))))
		(else #f)))


; 2.55
; so apparently 'x == (quote x)
; then ''x ==> (quote (quote x)) => '(quote x), ie a list of 2 terms, so car returns the first



(define (deriv exp var)
	(cond 
		((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
		((product? exp)
			(make-sum 
				(make-product (multiplier exp) (deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var) (multiplicand exp))))
		; modified by 2.56
		((exponentiation? exp)
			(make-product 
				(make-product 
					(exponent exp) 
					(make-exponentiation (base exp) (make-sum (exponent exp) -1))) 
				(deriv (base exp) var)))
		(else (error "Idk wtf you just gave me" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
(define (make-sum x y)
	(cond
		((=number? x 0) y)
		((=number? y 0) x)
		((and (number? x) (number? y)) (+ x y))
		(else (list '+ x y))))
(define (make-product x y) 
	(cond
		((or (=number? x 0) (=number? y 0)) 0)
		((=number? x 1) y)
		((=number? y 1) x)
		((and (number? x) (number? y)) (* x y))
		(else (list '* x y))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend x) (cadr x))
(define (augend x) 
	; modified by 2.57
	(if (> (length x) 3) (cons '+ (cddr x)) (caddr x))) ; if there are 3 terms, leave it, if there are more, return the sum of everything but the last
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier x) (cadr x))
(define (multiplicand x) 
	; modified by 2.57
	(if (> (length x) 3) (cons '* (cddr x)) (caddr x)))
(define (=number? exp number) (and (number? exp) (= exp number)))

; 2.56
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation x y)
	(cond
		((=number? y 0) 1)
		((=number? y 1) x)
		((=number? x 0) 0)
		((and (number? x) (number? y) (expt x y)))
		(else (list '** x y))))


; 2.57
; a) this is pretty easy, + and * are now just 2nd not first, and addend and multiplier are 1st and not 2nd
; ie
(define (make-sum-2 x y)
	(list x '+ y)) 
	; and then all the jazz about simplifying
(define (sum-2? x) (cadr x)) ; etc etc

; b)
; hmm
; not sure about all the implications about PEMDAS but
; can we make 
; (addend (list 'x '+ 'y '+ 2)) -> 'x
; (augend (list 'x '+ 'y '+ 2)) -> 'y '+ 2
(define a (list 'x '+ 'y '+ 2))
(define (addend-3 x)
	(car x))
; (define (augend-3 x)
; 	(cddr x))
; yes, we can pretty easily BUT
; (augend (list 'x '+ 'y)) will break things, because itll return a list.
; so we actually want
(define (augend-3 x)
	(if (> (length (cddr x)) 1) (cddr x) (caddr x)))
; and otherwise its pretty much the same
; i think



; sets!

(define (element-of-set? x set)
	(cond
		((null? set) #f)
		((equal? x (car set)) #t)
		(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
	(if (element-of-set? x set) set (cons x set)))

(define (intersection-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2)
			(cons (car set1) (intersection-set (cdr set1) set2)))
		(else (intersection-set (cdr set1) set2))))

; 2.59
(define (union-set s1 s2)
	(cond
		((null? s1) s2)
		((null? s2) s1)
		(else (adjoin-set (car s1) (union-set (cdr s1) s2)))))

; 2.60
; we dont need any changes?  we probably _could_ simplify adjoin to not bother checking element-of, which would speed it up,


