#!/usr/bin/racket
#lang racket


; 2.73
; a) We can dispatch on operator because they are constants regardless of their arguments
; But we can't use the value of a number as the type, because there are infinitely many
; We'd end up with 2 as the _type_ of 2, and 3 as a distinct type, the type of 3, etc
; Same with variable, infinitely many.
; But for + and *, the type of _all_ additions and multiplications is plus and times

b)
(define (install-addition-deriv)

	(define (make-sum x y)
		(cond
			((=number? x 0) y)
			((=number? y 0) x)
			((and (number? x) (number? y)) (+ x y))
			(else (tag (list '+ x y)))))

	(define (tag x) (cons '+ x))
	(put 'deriv '+ make-sum)
)

; similar for times
; c) doesn't seem useful
; d) ... change the puts around to match?  I don't understand the significance


; 2.74
;a) Isn't this also just 'hey did you read the last section?'
; HQ writes a function called (get-record EmpName).  HQ also keeps a list of all department names
; Each department writes a function called (get-record EmpName) and installs 
; it in the dispatch table keyed with their department.  This returns the typed record of the employee or false
; HQ's function iterates through the depts til it gets something not false.
; HQ then has some selectors, (get-salary Emp), etc, that each department also writes and installs in the dispatch table under their names

; So HQ can call (get-salary (get-record EmpName))
; and it will do 2 dispatches, one to get the emp (which is a loop over all types) and one to then, with the known type, get the information
; b)
(define (type-tag r) (car r))
(define (contents r) (cdr r))

(define (get-salary record)
	((get 'get-salary (type-tag r)) record))
; record must be ('type-tag [whatever you want])
; c)
; assume all department files are structured like
; ('dept-name (r1 r2 r3 r4, etc))
; also assume that each dept has a function as described in part a
(define (find-employee-record name . departments)
	(define (get-rec-of-dept dept)
		(get 'get-record dept))
	(define (get-dept-type dept) (car dept))

	(let (rec-or-false ((get-rec-of-dept (get-dept-type dep)) name))
		(if (rec-or-false)
			rec-or-false
			(find-employee-record name (cdr departments)))))
; d) just install the appropriate functions into the dispatch table
