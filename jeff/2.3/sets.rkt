(define (echo line) (display line) (newline))

; Exercise 2.59
; First, the text's mplementation of a set as an unordered list.
; This is linear in time...
(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
    (if (element-of-set? x set)  ; ... and, hence, this is too, ...
        set
        (cons x set)))
(define (intersection-set s1 s2)
    (cond ((or (null? s1) (null? s2)) '())
          ((element-of-set? (car s1) s2)  ; ... and this.
           (cons (car s1)
                 (intersection-set (cdr s1) s2)))
          (else (intersection-set (cdr s1) s2))))

; Now, my code.
; The union is pretty easy!
; We already have a set-membership check in adjoin-set.
; Leverage it:
(define (union-set s1 s2)
    (if (null? s1)
        s2
        (adjoin-set (car s1)
                    (union-set (cdr s1)
                               s2))))
; Quick tests:
(echo (union-set '() '()))
(echo (union-set '(1) '()))
(echo (union-set '() '(1)))
(echo (union-set '(1) '(1)))  ; No duplicates!
(echo (union-set '(1 2 3 4) '(3 4 5 6)))
(echo (union-set '(1 3 5 7) '(2 4 6 8)))
(echo (union-set '(2 4 6 8) '(1 3 5 7)))

; Exercise 2.60
; This is interesting, actually!
; A couple functions stays the same.
; Or, at least, can't be meaningfully reduced:
(define element-of-set? element-of-set?)
(define intersection-set intersection-set)
; Adjoin-set can be made constant-time:
(define (adjoin-set x set)
    (cons x set))
; And union-set can be made a bit more concise.
; There's even an improvement if we leave the code unchanged!
; Since adjoin-set is constant, we've gone quadratic -> linear, for free.
; But it's hard to resist:
(define (union-set s1 s2)
    (append s1 s2))
; But it's important to keep in mind: our sets are growing very large.
; Consider we want to:
;   1) Loop through every citizen in the United States, n.
;     1) Record their state of residence, k.
;     2) Check the set: have we seen this state before?
;       1) ...
;     3) Store the state in the set.
; With a non-duplicative set, the lookup in 1.2 checks, at worst, K, 50 states.
; With a duplicative set, it checks, at worst, N, something like 3e8 states.
; This particular algorithm would be quadratic, and not linear, in N.
; So, it might be said that this approach is inappropriate when
;   K) the size of our "set universe", is greatly overwhelmed by
;   N) the size of our "input universe".
; However, K is large, and we have *very few* collisions.
; Then the size difference between the two approaches won't be significant.
; We might appreciate having those quick appends!
; (But then why use a set?)
