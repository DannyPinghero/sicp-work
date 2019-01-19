#!/usr/bin/racket
#lang racket/base


;; 1.9
; (define (add1 a b)
; 	(if (= a 0)
; 		b
; 		(inc (add1 (dec a) b))
; 	)
; )

; (add1 4 5)
; => (inc (add1 (dec 4) 5))  => 1 + (3 + 5) => 1 + 1 + (2 + 5) => 1 + 1 + 1 + (1 + 5) => 1 + 1 + 1 + 1 + (0 + 5) => 1+1+1+1+5 = 9

; (define (add2 a b)
; 	(if (= a 0)
; 		b
; 		(+ (dec a) (inc b))
; 	)
; )

; (add 4 5)
; => 3 + 6 => 2 + 7 => 1 + 8 => 0 + 9 =>9





(define (A x y)
	(cond 
		((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1) (A x (- y 1))))
	)
)


;; 1.10
; 1024
; 65536
; 65536

(define (f n) (A 0 n))
; == 2 * n
(define (g n) (A 1 n))
; (A 1 2) = (A 0 (A 1 1)) = (A 0 2) = 4
; (A 1 3) = (A 0 (A 1 2)) = (A 0 4) = 8
; we know from the earlier example that (A 1 10) == 2**10
; g == 2**n
(define (h n) (A 2 n))
; (A 2 1) = 2 == 2 ** 1 == 2 ** (2 ** (1 - 1))
; (A 2 2) = (A 1 (A 2 1)) = (A 1 2) = 2 ** 2 == 2 ** (2 ** 2 - 1)
; (A 2 3) = (A 1 (A 2 2)) = (A 1 4) = 2 ** 4 == 2 ** (2 ** 4-1)
; (A 2 4) = (A 1 (A 2 3)) = (A 1 16) = 2 ** 16 == 2 ** (2 ** 5-1)
; (A 2 5) = (A 1 (A 2 4)) = (A 1 (2**16)) = 2 ** (2 ** 16)
;  h == 2 ** (2 ** (n-1))


;  1.11

(define (f2 n)
	(if (< n 3) 
    	n
     (+ (f2 (- n 1)) (* 2 (f2 (- n 2))) (* 3 (f2 (- n 3))))
    )
)


; f(4) = f(3) + 2f(2) + 3f(1)
; f(5) = f(4) + 2f(3) + 3f(2)

; n = 0
; f = 0
; n = 1
; f = 1
; n = 2
; f = 2

; n = 3
; f = f(2) + 2f(1) + 3f(0) = 2 + 2 + 0

; n = 4
; f = f(3) + 2*f(2) + 3f(1)

; n = 5
; f = f(4) + 2* f(3) + 3f(2)

; a = f(2) = 2
; b = f(1) = 1
; c = f(0) = 0

; a <- a + 2*b + 3*c
; b <- a
; c <- b

(define (f3 n)
  (f-iter 2 1 0 n)
  )
(define (f-iter a b c count)
  ; (display a)
  (if (< count 3)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))
      )
  )


; 1.13

; 1 <-row 0
; 1 1
; 1 2 2
(define (pascal-row-col row col)
  (cond 
    ((= row 0) 1)
    ((= row col) 1)
    ((= col 0) 1)
    (else (+ (pascal-row-col (- row 1) (- col 1)) (pascal-row-col (- row 1) col)))
   )
)
(define (pascal num-rows)
  (define (pascal-inner row col)
    (cond 
      ((> row num-rows) 0)
      ((> col row) ((newline)(pascal-inner (+ 1 row) 0)))
      (else ((display (pascal-row-col row col)) (display "|") (pascal-inner row (+ 1 col))))))
  (pascal-inner 0 0)
)

;  1.16
(define (fast-exp b n)
	(cond 
		((= n 0) 1)
  		((even? n) (square (fast-exp b (/ n 2))))
    	(else (* b (fast-exp b (- n 1))))
	)
)
(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))



(define (fast-exp-2 b n)
  (fast-iter-exp b n 1)
)

;  not sure about this guy  - yea def wrong
(define (fast-iter-exp b counter product)
  (if (= 0 counter)
      product
      (cond
		((even? counter) (* (fast-iter-exp b (/ counter 2) (* product (fast-iter-exp b (/ counter 2) product))))) 
		(else (* b (fast-iter-exp b (- counter 1) (* product b))))
      )
  )
)

;  1.17
(define (fast-mult a b)
  (cond
    ((= a 1) b)
    ((even? a) (double (fast-mult (halve a) b)))
    (else (+ b (fast-mult (- a 1) b))) ; 5 * 4 == 4*4 + 4
  )
)

(define (double n) (* n 2))
(define (halve n) (/ n 2))



; 1.18 ??

; 1.19


; TpqTpq (a,b) =>
; Tpq (Tpq (a,b)) =>
; Tpq (bq + aq + ap, bp + aq) =>
; ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, (bp + aq)p + (bq + aq + ap)q) =>

; (bpq + aqq + bqq + aqq + apq + bqp + aqp + app, bpp + aqp + bqq + aqq + apq) =>

; (b(2pq + qq) + a(qq + 2pq) + a(pp + qq), b(pp + qq) + a(2pq + qq)) =>

; p' = (pp + qq)
; q' = (2pq + qq)

(define (fib n)
  (fib-iter 1 0 0 1 n)
)
(define (fib-iter a b p q count)
	(cond
   		((= count 0) b)
     	((even? count) (fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ count 2)))
      	(else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))
	)
)





