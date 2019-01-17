; :)
(define echo (lambda (line) (display line) (newline)))
(define square (lambda (x) (* x x)))

; We're going to implement:
;   f(x, y) = xa^2 + yb + ab,
;   where a = 1 + xy
;         b = 1 - y
; in two *identical* ways.
; (The second is a shorthand for the first;
;  I've verified that both of these work.)

(define (f x y)
    ((lambda (a b)
        (+ (* x (square a))
           (* y b)
           (* a b)))
     (+ 1 (* x y))
     (- 1 y)))

(define (f x y)
    ; Notice that this is all one expression:
    ; the body is *in the let*!
    ; These (var exp) pairs are calculated before the body is executed,
    ; and are *not immediately visible*.
    ; That is, they're like assignments in Python:
    ;   x = 2
    ;   x, y = 1, x + 1  # x = 1, y = 3.
    ; and unlike C:
    ;   int x = 1, y = x + 1;  // x = 1, y = 2.
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
      (+ (* x (square a))
         (* y b)
         (* a b))))

(define (f x y)
    ; This third style is apparently frowned upon,
    ; for reasons that are unclear to me, as of yet.
    (define a (+ 1 (* x y)))
    (define b (- 1 y))
    (+ (* x (square a))
       (* y b)
       (* a b)))

; f(2, 3) = 78
(echo (f 2 3))

; Exercise 1.34
;   (f f) -> (f 2) -> (2 2)
; which is a name error of some sort,
; since 2 is not a function.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough a b)
        (< (abs (- a b)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough guess next)
                next  ; Why not guess?
                (try next))))
    (try first-guess))

(echo (fixed-point cos 1.0))

(define (average a b) (/ (+ a b) 2))
(define (sqrt x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(echo (sqrt 16))
(echo (sqrt 81))

; Exercise 1.35
;   phi^2 = phi + 1
;     => phi^2 - phi - 1 = 0
; This is just a quadratic!
; We're pretty confident this will intersect y=x at some point.
; We can just as easily conjure up a fixed point for phi:
;   phi^2 = phi + 1
;     => phi = 1 + 1/phi
; Not really sure how we derive the "starting point";
; here I'm using 1.0 since I obviously know the golden ratio ahead of time.
; I'm not... totally sure I understand why this works.
(define (golden-ratio)
    (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

(echo (golden-ratio))

; Exercise 1.36
; We're amending the fixed-point function to print out the guesses.
; This is made a bit awkward by the "fenceposting" going on with guess and next.
(define (fixed-point f first-guess)
    (echo "FIXED POINT:")
    (define (close-enough a b)
        (< (abs (- a b)) tolerance))
    (define (try guess)
        (echo guess)
        (let ((next (f guess)))
            (if (close-enough guess next)
                ((lambda () (echo next) next))  ; Ha... this can't be idiomatic.
                (try next))))
    (try first-guess))
; Then, we're implementing the fixed point for:
;   x^x = 1000
;     => logx(x^x) = logx(1000)
;     => x = logx(1000)
;     => x = ln(1000) / ln(x)  <-- change of base.
; We should notice some performance improvement with average damping.
; We can't start with first-guess=1.0 since that divides by log(1)=0.
; The answer is obviously going to be between 4 and 8, since:
;   4^4 = (2^2)^4 = 2^8 = 256
;   8^4 = (2^3)^4 = (2^10)(2^2) ~= 4000
; so let's just use first-guess=4.0.
(define (undamped-xx)
    (fixed-point (lambda (x) (/ (log 1000) (log x))) 4.0))
(define (damped-xx)
    (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 4.0))
; As expected, (damped-xx) uses many fewer steps.
; (It's something like O(log) steps actually, isn't it?)
(undamped-xx)
(damped-xx)
