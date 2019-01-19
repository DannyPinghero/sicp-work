#!/usr/bin/racket
#lang racket

; 2.44

(define (up-split painter n)
	(if (= n 0)
		painter
		(let ((smaller (up-split painter (- n 1))))
			(below painter (beside smaller smaller)))))

; 2.45
(define (split fst second)
	(lambda (painter n)
		(if (= n 0)
			painter
			(let (smaller ((split fst second) painter (- n 1)))
				(fst painter (second smaller smaller))))))

; 2.46

(define (make-vec x y)
	(cons x y))
(define (xcor-vect vec) (car vec))
(define (ycor-vect vec) (cdr vec))

(define (add-vect v1 v2)
	(make-vec
		(+ (xcor-vect v1) (xcor-vect v2))
		(+ (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
	(make-vec
		(* s (xcor-vect v))
		(* s (ycor-vect v))))
(define (sub-vect v1 v1)
	; ;)
	(add-vect v1 (scale-vect v2 -1)))


; 2.47
(define (origin-1 frame) (car frame))
(define (edge1-1 frame) (cadr frame))
(define (edge2-1 frame) (caddr frame))

(define (origin-2 frame) (car frame))
(define (edge1-2 frame) (cadr frame))
(define (edge2-2 frame) (cddr frame)) ; this is the only difference

; 2.48
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

; 2.49
; a - outline means go from 0,0 -> 0,1 ->1,1 ->1,0 -> 0,0
(define a (list 
	(make-segment (make-vec 0 0) (make-vec 0 1))
	(make-segment (make-vec 0 1) (make-vec 1 1))
	(make-segment (make-vec 1 1) (make-vec 1 0))
	(make-segment (make-vec 1 0) (make-vec 0 0))
	))
(define b (list
	(make-segment (make-vec 0 0) (make-vec 1 1))
	(make-segment (make-vec 0 1))
	))
(define c (list
	(make-segment (make-vec 0 0.5) (make-vec 0.5 1))
	(make-segment (make-vec 0.5 1) (make-vec 1 0.5))
	(make-segment (make-vec 1 0.5) (make-vec 0.5 0))
	(make-segment (make-vec 0.5 0) (make-vec 0 0.5))))

; d) yea not doing that

; 2.50
(define (flip-horiz painter)
	(transform-painter painter
		(make-vec 1 0)
		(make-vec 0 0)
		(make-vec 1 1)))
; the other two are not interesting enough to bother

; 2.51
(define (below low-painter high-painter)
	(let (
		(low (transform-painter low-painter
			(make-vec 0 0)
			(make-vec 1 0)
			(make-vec 0 0.5)))
		(high (transform-painter high-painter
			(make-vec 0 0.5)
			(make-vec 1 0.5)
			(make-vec 0 1)))
	(lambda (frame)
		(low frame)
		(high frame)))))

; we need to rotate the two pieces 90 degrees left, put them beside each other, and then rotate the whole thing 90 degrees right
(define (below2 p1 p2)
	(rotate-right (beside (rotate-left p1) (rotate-left p2))))
	)

; 2.52 not really a thing worth doing
