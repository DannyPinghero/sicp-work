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

; 2.61
(define (adjoin-ordered-set set element)
	; if the element is smaller than the first guy in the set, great new set is (element *set)
	; otherwise compare element to the next guy in the set.  BUT we have to keep the car of the set around too hm
	; so we need a way to sandwich an element between two lists
	; ok, fine, given left, el, right: (append left (cons el right))
	(define (inner end-of-set element start-of-set)
		(cond
			((null? end-of-set) 
				(append start-of-set (list element)))
				; if end-of-set is empty, then element is larger than everything in the set
			((< element (car end-of-set))
				; if its smaller than the 1st element of the right hand set, then great, this is where it goes
				(append start-of-set (cons element end-of-set)))
			((= element (car end-of-set))
				; if its already in the set, then all done
				(append start-of-set end-of-set))
			(else 
				(inner (cdr end-of-set) element (append start-of-set (list (car end-of-set)))))))
	(inner set element '()))

; (define ordered-set '(1 2 3 5))
; (adjoin-ordered-set ordered-set 4)
; (adjoin-ordered-set ordered-set 6)
; (adjoin-ordered-set ordered-set 1)


; 2.62

(define (union-ordered-set s1 s2)
	(cond
		((null? s1) s2)
		((null? s2) s1)
		(else 
			(let ((x1 (car s1)) (x2 (car s2)))
				(cond
					((= x1 x2)
						; if both elements are equal, then we only need one of them, so take the first one, cdr down _both_ sets
						(cons x1 (union-ordered-set (cdr s1) (cdr s2))))
					((< x1 x2)
						(cons x1 (union-ordered-set (cdr s1) s2)))
					(else
						(cons x2 (union-ordered-set s1 (cdr s2)))))))))
; only real difference from the intersection in the book is that we keep the small element instead of throwing it away

; (define s1 '(1 2 3 4))
; (define s2 '(2 4 6 8))
; (define s3 '(1 3 5 7))
; (union-ordered-set s1 s1)
; (union-ordered-set s1 s2)
; (union-ordered-set s1 s3)
; (union-ordered-set s2 s3)


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-tree-set? x set)
	(cond
		((null? set) #f)
		((= x (entry set)) #t)
		((< x (entry set))
			(element-of-tree-set? x (left-branch set)))
		((> x (entry set))
			(element-of-tree-set? x (right-branch set)))))

(define (adjoin-tree-set x set)
	(cond
		((null? set) (make-tree  '() '()))
		((= x (entry set)) set)
		((< x (entry set))
			(make-tree 
				(entry set)
				(adjoin-tree-set x (left-branch set))
				(right-branch set)))
		((> x (entry set))
			(make-tree
				(entry set)
				(left-branch set)
				(adjoin-tree-set x (right-branch set))))))

; 2.63
; tree to list 1
; if null, return null
; otherwise make a list that is:
; (tree-to-list left-branch, cur value, tree-to-list of right-branch)
; ie this is an in order walk

; tree to list 2
; if null return null
; otherwise 
; take the left branch and hold on to it
; exhaust the right branch, turning it into a list, then put cur value in the front, then recurse with the left branch
; i think?

; lets see some examples
; consider

; 			7
; 		3		9
; 	1		5		11


; t2l1 will do
; t2l T3 7 T9 -> t2l T1 3 T5 7 T5 9 T11 -> 1 3 5 7 9 11

; t2l2 will do
; t2l2(T7, ()) -> t2l2(T3, (7 t2l2(T9,())))
; => t2l2(T3, (7, 5, 9, 11))
; => t2l2(T1, (3, t2l2(T5, (7, 5, 9, 11))))
; => t2l2(T1, (3, 5,7,5,9,11))
; => t2l2((), (1 t2l2((), (3,5,7,5,9,11))))
; => t2l2((), (1,3,5,7,5,9,11)) => (1,3,5,7,5,9,11)
;  hmm dupe 5.  does that happen or did i fuck up.  otherwise they are the same  ah no i just fucked up, ok.  thought 5 was a child of 2 nodes

; t2l2(T5, (7, 5, 9, 11)) =>
; 	t2l2((), (5, t2l2((), (7, 5, 9, 11))))
; 	=> t2l2((), (5, 7,5,9,11)) => (5,7,5,9,11)

; t2l2(T9, ()) =>
; 	t2l2(T5, (9, t2l2(T11, ())))
; 	=> t2l2(T5, (9, 11)) => (5, 9, 11)

; t2l2(T11, ()) =>
; 	t2l2((), (11, t2l2((), ()))) =>
; 	t2l2((), (11, ())) => (11, ()) => (11)  (because im shorthanding cons, (cons 11, ()) is (11))

; t2l2(T5, (9, 11)) =>
; 	t2l2((), (5, t2l2((), (9,11))))  => t2l2((), (5, 9, 11))
; 	=> (5, 9, 11)



; consider 
; 		3
; 	1	|	7
; 		|5	|	9
; 		|		|	11
; t2l1(T3) 
; -> (t2l1(T1), 3, t2l1(T7))
; -> (((), 1, ()), 3, (t2l1(T5), 7, t2l1(T9)))
; -> (1, 3, ((), 5, ()), 7, ((), 9, t2l1(T11)))
; -> (1, 3, 5, 7, 9, (), 11, ()) -> 1,3,5,7,9,11

; t2l2:
; 	t2l2(T3, ()) ->
; 	t2l2(T1, (3, t2l2(T7, ())))
; 	-> t2l2(T1, (3, 5, 7, 9, 11))
; 	t2l2((),(1, t2l2((), 3,5,7,9,11)))
; 	-> 1,3,5,7,9,11

; t2l2(T7, ()) ->
; 	t2l2(T5, (7, t2l2(T9, ())))
; 	-> t2l2(T5, (7, 9, 11))
; 	-> t2l2((), t2l2(5, (t2l2((), 7, 9, 11)))) -> 5,7,9,11

; t2l2(T9, ()) ->
; 	t2l2((), (9, t2l2(T11, ())))
; 	-> t2l2((), (9, 11))

; t2l2(T11, ()) -> t2l2((), (11, t2l2((), ())))
; 	-> (11, ()) -> (11)


; i still dont have a greatttt intuition for how t2l2 works
; basically recurse down the right branch, then the left
; pretty sure they both always result in an in order traversal


; 2.64
; consider 1 2 3 4 5 
; length is 5
; left size is 2
; left result is then a recursive call with just those 2
; this-entry is 3
; right-tree is a recursive call with the other 2

; so basically we split into 3 chunks, the left side, middle, and right side
; build a balanced tree of the left and right, and then join em to the middle
; its balanced because.....hmm we always take the same number of elements for each side, but does that prevent chains?  yes, passing 3 elements will always result in a proper triangle


; 2.65
; Since tree-to-list does in order walk, it will turn a tree into an ordered list
; then we can just use the already written intersect and union of ordered set
; and then i guess back to a tree

; 2.66
; assume each node of the tree is a list like (key value), and that the tree is built like
; in the previous sections (node, left, right) recursively
; (define (lookup-tree needle records)
; 	(let ((listified-tree (tree-to-list records)))
; 		(if (equal? needle (caar listified-tree))
; 			(car listified-tree)
; 			(lookup-tree needle (cdr listified-tree)))))
; though we can do better, actually, why destroy the search tree properties
; assume we've run the keys through list->tree
; (define (lookup-tree-good needle record-tree)
; 	(cond
; 		((null? record-tree) #f)
; 		((equal? needle (caar record-tree)) (car record-tree))
; 		; ((key val) (left list) (right list))
; 		((< needle (caar record-tree)) (lookup-tree-good needle (cadr record-tree)))
; 		(else (lookup-tree-good needle (caddr record-tree)))))


(define (make-leaf symbol weight)
	(list 'leaf symbol weight))
(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list 
		left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))
(define (hleft-branch tree) (car tree))
(define (hright-branch tree) (cadr tree))
(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))
(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)))

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond 
		((= bit 0) (hleft-branch branch))
		((= bit 1) (hright-branch branch))
		(else (error "nice try buddy" bit))))

(define (hadjoin-set x set)
	(cond
		((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set) (hadjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs) '()
		(let ((pair (car pairs)))
			(hadjoin-set (make-leaf (car pair) (cadr pair))
				(make-leaf-set (cdr pairs))))))


; 2.67
(define sample-h-tree 
	(make-code-tree 
		(make-leaf 'A 4)
		(make-code-tree
			(make-leaf 'B 2)
			(make-code-tree 
				(make-leaf 'D 1)
				(make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-h-tree)
; message reads: A D A B B C A

; 2.68
(define (encode message tree)
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
			(encode (cdr message) tree))))
(define (encode-symbol sym tree)
	(define (inner sym tree bits)
		(let (
			(left (hleft-branch tree))
			(right (hright-branch tree)))
			(cond
				((leaf? tree) bits)
				((element-of-set? sym (symbols left)) (inner sym left (append bits (list 0))))
				((element-of-set? sym (symbols right)) (inner sym right (append bits (list 1)))))))
	(inner sym tree '()))

(encode (list 'A 'D 'A 'B 'B 'C 'A) sample-h-tree)
; ciphertext reads (0 1 1 0 0 1 0 1 0 1 1 1 0).  victory!

; 2.69
(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

(define (successive-merge ordered-leaves)
; we have to take the two smallest weights, 
; call make code tree on them,
; and replace them in the list with the new tree
; we know the list is ordered, so starts off easy enough
	(cond 
		((= 1 (length ordered-leaves)) (car ordered-leaves))
		(else
			(let (
				(one (car ordered-leaves))
				(two (cadr ordered-leaves))
				(leftover (cond
					((> (length ordered-leaves) 2) (cddr ordered-leaves))
					(else '()))))
				(let (
					(new-node (make-code-tree one two)))
				; arbitrary which is the left and which the right

					(successive-merge (hadjoin-set new-node leftover)))))))

(define sample-leaves (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define sample-h-tree-2 (generate-huffman-tree sample-leaves))
(decode (encode (list 'A 'D 'A 'B 'B 'C 'A) sample-h-tree-2) sample-h-tree-2)
; nailed it

; 2.70
(define rock-leaves (list (list 'a 2) (list 'boom 1) (list 'get 2) (list 'job 2) (list 'na 16) (list 'sha 3) (list 'yip 9) (list 'wah 1)))
(define rock-tree (generate-huffman-tree rock-leaves))
(define rock-song (list 'get 'a 'job 'sha 'na 'na 'na 'na 'na 'na 'na 'na 'get 'a 'job 'sha 'na 'na 'na 'na 'na 'na 'na 'na 'wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'sha 'boom))
(display "message length: ")
(display (length rock-song)) ; 36
(newline)
(display "encoded length: ")
(display (length (encode rock-song rock-tree))) ; 84
(newline)
(display "fix length encoding length: ")
(display (* 3 (length rock-song))) ; 108
(newline)

; 2.71
a1 b2 c4 d8 e16
			ab3			c4 d8 e16
		a			b

			abc7		d8 e16
		ab3  |  c4
	a	|	b

				abcd8
			abc7  |  d8
			etc

				abcde
			abcd  |  e16

most frequent symbol, e, takes just 1 bit (a 1)
least, the a or b, takes 4 0001 or 0000

in general there'll be n levels, so n-1 bits for the least frequent symbol

; 2.72
 ; -


