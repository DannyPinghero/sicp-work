#!/usr/bin/racket
#lang racket
(require racket/include)
(include "gcd.rkt")
; (define (square x) (* x x))
; filter is already built in
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence)
			(accumulate op initial (cdr sequence)))))


; 2.33
(define (map2 p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) null sequence))
; the op in accumulate gets 2 args, car and accum of cdr.  for map, just operate on the car

; (map square (list 1 2 3 4 5))
; (map2 square (list 1 2 3 4 5))

(define (append2 seq1 seq2)
	(accumulate cons seq2 seq1))
; (append (list 1 2 3) (list 4 5 6))
; (append2 (list 1 2 3) (list 4 5 6))

(define (length2 sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))
; hmm
; () -> 0, hits the null branch
; (1) -> f(1, 0) => 1 + 0 = 1
; (1 2) -> f(1 length2((2))) -> f(1, f(2, length2(()))) -> f(1, f(2, 0)) => f(1, (1+0)) -> f(1, 1) -> 1 + 1 = 2

; (length (list 1 2 3 4 5))
; (length2 (list 1 2 3 4 5))

; 2.34
(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
		0
		coefficient-sequence))
; (horner-eval 2 (list 1 3 0 5 0 1))
; 1 + 6 + 5*8 + 32 = 79

(define (enumerate-tree tree)
	(cond ((null? tree) null)
			((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
			(enumerate-tree (cdr tree))))))
; 2.35
(define (count-leaves t)
	(accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
; enumerate tree flattens  tree into a list
; then we change that list into all 1s, and add em up
; had to look this up

; 2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		null
		(cons (accumulate op init (map (lambda (l) (car l)) seqs))
			(accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))

; (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; 2.37
(define (dot-product v w)
	(accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
	(map (lambda (row) (dot-product row v)) m))
; matrix * vector is just dot product each row with vector
(define (transpose mat)
	(accumulate-n cons null mat))
; we want to take the first column and make it a first row
; thats basically what accumulate n does already
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
	(map (lambda (row) (matrix-*-vector cols row)) m)))
; M*N is basically dot product of each row with each col of N, aka each row of N transpose

; 2.38
; 3 / 2 / 1 -> 1.5
; 1 / 2 / 3 -> 1/6
; (1 (2 (3 ())))
; (((() 1) 2) 3)
; Commutativity I assume
(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest))
				(cdr rest))))
	(iter initial sequence))
; 2.39
(define (reverse-r sequence)
	(accumulate (lambda (x y) (append y (list x))) null sequence))
; (reverse-r (list 1 2 3 4 5))
(define (reverse-l sequence)
	(fold-left (lambda (x y) (cons y x)) null sequence))
; does work, not 100% clear on it though
; (reverse-l (list 1 2 3 4 5))

; 2.40
(define (enumerate-interval low high)
	(if (> low high)
		null
		(cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
	(accumulate append null (map proc seq)))
(define (unique-pairs n)
	(flatmap (lambda (x) (map (lambda (y) (list x y)) (enumerate-interval 1 x))) (enumerate-interval 1 n)))
(define (make-pair-sum pair)
	(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
	(filter prime-sum? (map make-pair-sum (unique-pairs n))))

; 2.41
(define (unique-triples n)
	; for each number 1 - n
	; generate all pairs up to that number, and prepend that number to it
	; remember that 
	; map takes a list of numbers and a function that takes one number, and turns each number in the list into a new number, and gives you a new list
	; flatmap takes a list of numbers a function that takes one number and turns each number into a _list_, and then flattens out the list of lists
	;  so here each number 1-n yields a list of triples
	(flatmap 
		(lambda (third-number) (map 
			(lambda (pair) (cons third-number pair))
			(unique-pairs third-number)))
		(enumerate-interval 1 n)))
(define (triple-sum n s)
	(filter 
		(lambda (triple) (= s (+ (car triple) (cadr triple) (caddr triple))))
		(unique-triples n)
	))

; 2.42
; some help from here in understanding the arguments id be given
; http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html


(define (make-position r c) (cons r c))
(define (get-row pos) (car pos))
(define (get-col pos) (cdr pos))
(define empty-board null)

(define (adjoin-position row col positions)
	(append positions (list (make-position row col))))

(define (safe? col positions)
	; this is going to get passed a col, a number, and a list of lists, each inner list is r,c of a queen
	; we want to know if the queen in col if in danger

	; we need to check if anything else is in the same row
	; or if anything is diagonally aligned
	; what does diagonal mean?
	; if you are in 1,1, then 2,2 or 3,3 or etc is attacking you
	; if you are in 1,2 then 2,3 or 3,4 etc is attacking you
	; so if you are in pos r,c then for all n, r-n, c-n is diagonal to you
	; put another, if i subtract two positions, and the row and col is different by the same abs number, they are diagonal

	; what row are we talking about?
	(define cur-queen
		(car (filter
				(lambda (pos) (= col (get-col pos)))
				positions
			)))
	; are there any queens in my row?
	(define (queens-in-my-row) 
		; more than 1 because cur-queen is in the positions list
		(< 1 (length 
			(filter 
				(lambda (occupied-pos)
					(= (get-row occupied-pos) (get-row cur-queen)))
				positions))))
	; am i in danger diagonally?
	(define (queens-in-my-diag)
		; more than 1 because cur-queen is in the positions list
		(< 1 (length
			(filter
				(lambda (occupied-pos)
					(let (
						(row-offset (abs (- (get-row occupied-pos) (get-row cur-queen))))
						(col-offset (abs (- (get-col occupied-pos) (get-col cur-queen)))))
					(= row-offset col-offset)))
				positions))))
	; i need there to be (not queens in my row) AND (not queens in my diag)
	; which by de morgan is not (queens in my row OR queens in my diag)
	(not (or (queens-in-my-row) (queens-in-my-diag)))
)

(define (queens board-size)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
					(lambda (rest-of-queens)
						(map (lambda (new-row)
								(adjoin-position new-row k rest-of-queens))
							(enumerate-interval 1 board-size)))
					(queen-cols (- k 1))))))
	(queen-cols board-size))

; 2.43
; /shrug hes a dummy idk what to tell you
