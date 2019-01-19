#!/usr/bin/racket
#lang racket

;;1.1
; 10
; 12
; 8
; 3
; 4
; nil ; a = 3
; nil ; b = 4
; 19
; false
; 4 ; first condition is true
; 16 ; middle branch is true
; 6 ; first condition is true
; 16 ; middle branch is true


; ;; 1.2

; 5+ 1/2 + (2 - (3 - (6 + 1/3)))
; ______________________________
; 3 * (6 - 2) * (2 - 7)


; (/ (+ 5 (/ 1 2) (- 2 (- 3 ( + 6 (/ 1 3))))) (* 3 (- 6 2) (- 2 7))) ;; -13 / 72 == -.1805555...

;; 5 + 0.5 + (2 - (3 - (6 + 1/3))) = 10.833333
;; 3*(6-2)*2-7) = -60


;; 1.3
(define (sum-square a b)
    (+ (* a a) (* b b))
)

(define (sum-other-two main a b)
    (cond
        ((> a b)
            (sum-square main a)
        )
        (else
            (sum-square main b))
    )
)

(define (sum-larger-square num1 num2 num3)
    (cond
        ((and (> num1 num2) (> num1 num3))
            (sum-other-two num1 num2 num3))
        ((and (> num2 num1) (> num2 num3))
            (sum-other-two num2 num1 num3))
        ((and (> num3 num1) (> num3 num2))
            (sum-other-two num3 num1 num2))
    )
)

(sum-larger-square 10 2 3)

(sum-larger-square 1 2 3)

;; 1.4

; If b is positive, it adds them, otherwise it subtracts them, thus doing a + |b|

; ;; 1.5
; If applicative order, if will fail because parameter y has to be evaluated first, and thats an invalid circle
; If normal order, since paramter x is 0, the first branch of the if is evaluated and the fact that p is bottom doesn't matter

