#!/usr/bin/racket
#lang racket

(define (square x) (* x x))
(define odds (list 1 3 5 7 9))
(define squares (list 1 4 9 16 25))
; 2.17
(define (last-pair l)
	(cond
		((null? l)(display "List must be non empty"))
		((= (length l) 1) l)
		(else (last-pair (cdr l)))))
; 2.18
(define (reverse l)
	(if (= (length l) 1)
		l
		(append (reverse (cdr l)) (list (car l)))))
; 2.19
; eh
; 2.20
(define (same-parity fst . l)
	(define (inner initial-list par acc)
		(if (null? initial-list) 
			(reverse acc)
			(if 
				(= par (remainder (car initial-list) 2)) 
				(inner (cdr initial-list) par (cons (car initial-list) acc))
				(inner (cdr initial-list) par acc)
			)))
	(if 
		(null? l)
		(display "Hey!")
		(inner l (remainder fst 2) (list fst))))
; 2.21
(define (square-list1 items)
	(if (null? items)
		null
		(cons (square (car items)) (square-list1 (cdr items)))
		))
(define (square-list2 items)
	(map square items))

; 2.22
; square-list (1 2 3 4)
; -> iter (2 3 4) (cons 1 ())
; -> iter (3 4) (cons 4 (1))
; -> iter (4) (cons 9 (4 1)), etc
; reversed because he pushes the squares onto the list

; then he tries
; (square-list (1 2 3 4))
; -> iter (2 3 4) (cons () 1)
; -> iter (3 4) (cons (() 1) 4), etc
; cons doesnt work right unless its element, list
; he could have used append and list here, to get to the end

(define (for-each f l)
	(cond ((null? l) (newline))
		(else (f (car l)) (for-each f (cdr l)))))

; 2.24
; This is a list of 2 elements.  They are
; 1 and a list of 2 elements.  They are
; 	2 and a list of 2 elements They are
; 		3 and 4

; 2.25
(define la (list 1 3 (list 5 7) 9))
(define lb (list (list 7)))
(define lc (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdaddr la))
(caar lb)
(cadadr (cadadr (cadadr lc)))

; 2.26
; -> (1 2 3 4 5 6 )
; -> ((1 2 3) 4 5 6)
; -> ((1 2 3) (4 5 6))

; 2.27
(define lx (list (list 1 2) (list 3 4)))

(define (deep-reverse l)
	(cond 
		((null? l) null)
		((pair? (car l)) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
		(else (append (deep-reverse (cdr l)) (list (car l))))))

; 2.28
(define tx (list (list 1 2) (list 3 4)))
(define (fringe tree)
	(cond
		((null? tree) null)
		((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
		(else (append (list (car tree)) (fringe (cdr tree))))))
; 2.29

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch)) ; this is always a number
(define (branch-structure branch) (car (cdr branch))) ; this is eiher a number, the weight, or another mobile

(define (total-weight mobile)
	(define (weigh-branch branch acc)
		(cond ((pair? (branch-structure branch)) (+ acc (total-weight (branch-structure branch))))
			(else (+ acc (branch-structure branch))))
		)
	(+ (weigh-branch (left-branch mobile) 0) (weigh-branch (right-branch mobile) 0)))

(define (balanced? mobile)
	(let (
			[lb (left-branch mobile)]
			[rb (right-branch mobile)])
		(let (
			[lbw (if (pair? (branch-structure lb)) (total-weight lb) (branch-structure lb))]
			[rbw (if (pair? (branch-structure rb)) (total-weight rb) (branch-structure rb))])
			(= (* (branch-length lb) lbw) (* (branch-length rb) rbw)))))

; d) we'd just have to change the accessors to be a little more careful

; 2.30
(define (square-tree1 tree)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
			(square-tree1 sub-tree)
			(square sub-tree)))
	tree))

; not right, leaves extra null elements
(define (square-tree2 tree)
	(cond 
		((null? tree) null)
		((pair? (car tree))
				(append (square-tree2 (car tree)) (square-tree2 (cdr tree))))
		(else (list (square (car tree)) (square-tree2 (cdr tree))))))
; 2.31
(define (tree-map func tree)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
			(tree-map func sub-tree)
			(func sub-tree)))
	tree))
(define (square-tree3 tree) (tree-map square tree))

; 2.32
(define (subsets s)
	(if (null? s)
		(list null)
		(let ((rest_ (subsets (cdr s))))
			(append rest_ (map 
				(lambda (a-list) (cons (car s) a-list))
				rest_)))))

; if the given list is 1 element A (then the answer is (null, A), return a list that is:
; 	null
; 	that element + null == that element
; if the given list is 2 elements A B (then the answer is null, A, B, A B), return a list that is:
; 	the subsets of the tail, ie (null, B)
; 	and then each of those elements together with A, so
; 	null + A, B + A => null, A, B, A+B
; etc

