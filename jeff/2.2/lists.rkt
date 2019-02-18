(define (echo line) (display line) (newline))

; Exercise 2.17
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))
(echo (list 34))
(echo (last-pair (list 23 72 149 34)))

; Exercise 2.18
; This has a trivial recursive solution:
;   reverse([1, 2, 3, 4])
;     = reverse([2, 3, 4]) + [1]
;     = reverse([3, 4]) + [2, 1]
;     = reverse([4]) + [3, 2, 1]
;     = [4, 3, 2, 1]
; which can't be implemented efficiently,
; since there's no obvious way to append a constant to a list
; without rebuilding the entire list...
; (It's trivial to *prepend* to a list: (cons elem l).
;  If we stored the tail in car and the head in cdr, we could do (cons l elem);
;  but we don't.)
(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y))))
(define (reverse x)
    (if (null? x)
        `()
        (append (reverse (cdr x)) (list (car x) ))))
(echo (reverse (list 1 4 9 16 25)))

; Exercise 2.19
(define (no-more? coins) (null? coins))
(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+ (cc amount
                   (except-first-denomination coin-values))
               (cc (- amount (first-denomination coin-values))
                   coin-values)))))
(define us-mint (list 1 5 10 25 50))
(echo (cc 100 us-mint))

; Exercise 2.20
(define (same-parity head . tail)
    (define (filtered pred x)
        (cond ((null? x) `())
              ((pred (car x)) (cons (car x) (filtered pred (cdr x))))
              (else (filtered pred (cdr x)))))
    (cons head (filtered (if (even? head) even? odd?) tail)))
(echo (list 1 3 5 11))
(echo (same-parity 1 3 5 2 4 8 11))

; Exercise 2.21
(define (square-list items)
    (define (square x) (* x x))
    (if (null? items)
        `()
        (cons (square (car items)) (square-list (cdr items)))))
(echo (square-list (list 1 2 3 4 5)))
(define (map proc items)
    (if (null? items)
        `()
        (cons (proc (car items)) (map proc (cdr items)))))
(define (square-list items)
    (map (lambda (x) (* x x)) items))
(echo (square-list (list 1 2 3 4 5)))

; Exercise 2.22
; This is exactly the same problem we had above.
; Scheme's list representation is optimized for *prepending* elements.
; When we call:
;   (iter (cdr things)
;         (cons (square (car things))
;               answer))))
; we are prepending the element to the existing answer:
;   (1, `())
;     => (2, (1 `()))
;     => (3, (4, 1, `()))
;     => (4, (9, 4, 1, `()))
;     => ...
; This isn't what we want; we want to *append*.
; But we can't do this easily.
(define (square-list items)
    (define (square x) (* x x))
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (square (car things))
                        answer))))
    (iter items '()))
(echo (square-list (list 1 2 3 4 5)))
; This solution is actually perfectly valid!
; It's just not something that Scheme recognizes as a list.
; (Because we've swapped the meaning of car and cdr.)
(define (square-list items)
    (define (square x) (* x x))
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer
                        (square (car things))))))
    (iter items '()))
(echo (square-list (list 1 2 3 4 5)))

; Exercise 2.23
(define (for-each f items)
    (cond ((null? items) true)
          (else (f (car items))
                (for-each f (cdr items)))))
(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))
