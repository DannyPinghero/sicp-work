(define (echo line) (display line) (newline))

; Exercise 2.7
; It's tempting to "sniff out" the proper interval endpoints:
;   (define (lower-bound i) (min (car i) (cdr i)))
;   (define (upper-bound i) (max (car i) (cdr i)))
; But this isn't in keeping e.g. with their implementation of mul-interval,
; where they've taken great pain to pass (min, max) in the correct order.
; So:
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

; Exercise 2.8
; Have to be a bit careful here:
;   (-10, -1) - (-9, 5)
;     => (-10 - (-9), -1 - 5)
;     => (-1, -6)
; That is:
;   (lower-bound (sub-interval a b))
;     != (- (lower-bound a) (lower-bound b))
; But do we have to check every permutation? Probably not.
; Let's consider all the cases in the above example:
;   (-10, -1) - (-9, 5)
;     => ((- -10 5), (- -1 -9))
;     => (-15, 8)
; Notice, we omitted some pointless combinations, e.g.:
;   (- -10   5) <= (- -10  -9)
;   (-  -1  -9) >= (-  -1   5)
; That is:
;   small number - large number
;     <= small number - small number
; And, by symmetry:
;   large number - small number
;     >= large number - large number
(define (sub-interval a b)
    (make-interval (- (lower-bound a) (upper-bound b))
                   (- (upper-bound a) (lower-bound b))))
(define a (make-interval -10 -1))
(define b (make-interval  -9  5))
(define s (sub-interval a b))
(echo (lower-bound s))
(echo (upper-bound s))

; Exercise 2.9
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))
; It's obviously true for addition:
;   I  = (l, u)
;   WI = (I.u - I.l) / 2
;   S  = (A.l + B.l, A.u + B.u)
;   WS = ((A.u + B.u) - (A.l + B.l)) / 2
;      = (A.u - A.l) / 2 + (B.u - B.l) / 2
;      = WA + WB
; Let's demonstrate:
(define (add-interval a b)
    (make-interval (+ (lower-bound a) (lower-bound b))
                   (+ (upper-bound a) (upper-bound b))))
(define (smart-add-width a b)
    (+ (width a) (width b)))
(define (naive-add-width a b)
    (width (add-interval a b)))
(echo (smart-add-width a b))
(echo (naive-add-width a b))
; Then suppose:
;   P  = (A.l * B.l, A.u * B.u)
;   WP = (A.u * B.u - A.l * B.l) / 2
; This is just one possible case of mul-interval,
; in which WP pretty obviously can't be decomposed in terms of WA and WB.

