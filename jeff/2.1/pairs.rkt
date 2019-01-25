(define (echo line) (display line) (newline))

; Behold: a procedural implementation of pairs!
; I'm using the opportunity to brush up on my lambdas;
; the text uses an inner function here.
; I am allowing this implementation to override std const in this file.
; (So it better work!)
(define (car z) (z 0))
(define (cdr z) (z 1))
(define (cons x y)
    (lambda (m)
        (cond ((= m 0) x)
              ((= m 1) y)
              (else (error "Argument not 0 or 1 -- CONS" m)))))
(define foobar (cons "foo" "bar"))
(echo (car foobar))
(echo (cdr foobar))

; Exercise 2.4
; Ha! This is batshit.
;   (car (cons x y))
;     => (car (lambda (m) (m x y)))
;     => ((lambda (m) (m x y)) (lambda (p q) p))
;     => ((lambda (p q) p) x y)
;     => x
;   (cdr (cons x y))
;     => (cdr (lambda (m) (m x y)))
;     => ((lambda (m) (m x y)) (lambda (p q) q))
;     => ((lambda (p q) q) x y)
;     => y
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
(define foobar (cons "foo" "bar"))
(echo (car foobar))
(echo (cdr foobar))

; Exercise 2.5
; Encode a nonnegative pair as:
;   (a, b) -> p = 2^a * 3^b
; It's tempting to say:
;   p = 2^a * 3^b
;     => a = log2(p / 3^b)
;     => b = log3(p / 2^a)
; But we don't know a and b!
; (And encoding them in a const, say, defeats the purpose of the exercise.)
; But wait!
; It's obvious that p is going to be divisible by 2 and 3.
; And, just playing around in Python, it *seems* to be that case that:
;   1) there is no i such that 3^i is divisible by 2.
;   2) there is no j such that 2^j is divisible by 3.
; Why?
; It has to be related to the fact that they're prime...
; Actually, can I prove it?
; Suppose:
;   3^i = 2 * k
; Then:
;   3 * 3 * 3 * ... = 2 * k
;   ^----- i -----^   v--- i - 1 ---v
;     => 3 = 2 * k / (3 * 3 * 3 * ...)
;     => 3 = 2 * k'
;     => 3 is composite.
; Contradiction!
; Cool. So:
;   1) car => divide by 3 while divisible by 3, then log2(z).
;   2) cdr => divide by 2 while divisible by 2, then log3(z).
(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car z)
    (if (= (remainder z 3) 0)
        (car (/ z 3))
        (log z 2)))
(define (cdr z)
    (if (= (remainder z 2) 0)
        (cdr (/ z 2))
        (log z 3)))
(define foobar (cons 2 3))
(echo (car foobar))
(echo (cdr foobar))
(define foobar (cons 237 994))
(echo (car foobar))
(echo (cdr foobar))

