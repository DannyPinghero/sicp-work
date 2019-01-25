; The rational-number {construct,select}ors defined in the text.
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))
; A subset of arithmetic operators implemented in the text.
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
              (* (denom x) (denom y))))
(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
; A rational-number selector that represents (n/d) in lowest terms.
(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))

; Exercise 2.1
; Amend make-rat to handle negative numbers.
; Encode the sign *in the numerator*!
; Going to do this without a cond expression by calculating the sign as:
;   MAX(-1, MIN(n*d, 1))
; And returning
;   sign * ABS(n) / ABS(d)
; Consider the following cases:
;   1) n*d < 0:
;        => MAX(-1, MIN(n*d < 0, 1))
;        => MAX(-1, n*d < 0)
;        => -1
;   2) n*d > 0:
;        => MAX(-1, MIN(n*d > 0, 1))
;        => MAX(-1, 1)
;        => 1
;   3) n*d = 0:
;        => MAX(-1, MIN(0, 1))
;        => MAX(-1, 0)
;        => 0
; Case 3 might look wrong at first.
; But consider that n*d = 0 if and only if n=0, since d=0 => INF is impossible.
; Then it is harmless to multiply n=0 by sign=0.
(define (make-rat n d)
    (let ((np (abs n))
          (dp (abs d)))
        (let ((g (gcd np dp)))
            (cons (* (max -1 (min (* n d) 1))
                     (/ np g))
                  (/ dp g)))))
; A couple test cases, by no means exhaustive...
(print-rat (make-rat -18 20))  ; -18/20 => -9/10.
(print-rat (make-rat -8 -4))  ; -8/-4 => 8/4 => 2/1.
(print-rat (make-rat 8 -50))  ; 8/-50 => -8/50 => -4/25.
(print-rat (make-rat 0 10))  ; 0/10 => 0/1 = 0.
