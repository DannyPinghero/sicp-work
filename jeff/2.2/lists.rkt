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
(define (reverse x)
    (define (iter y i)
        (if (null? (cdr y))
            (cons (car y) i)
            (iter (cdr y) (cons (car y) i))))
    (iter x `()))
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

; Exercise 2.24
;   (list 1 (list 2 (list 3 4)))
;     => (1 (2 (3 4)))
;        /  \
;       1   (2 (3 4))
;           /  \
;          2   (3 4)
;              /   \
;             3     4
;              cdr      car
;     => -> [ 1 * ] -> [ * / ]
;            car         |      cdr      car
;                        +-> [ 2 * ] -> [ * / ]
;                             car         |      cdr
;                                         +-> [ 3 * ] -> [ 4 / ]
;                                              car        car

; Exercise 2.25
(define one (list 1 3 (list 5 7) 9))
; (1 3 (5 7) 9)
; cdr => (3 (5 7) 9)
; cdr => ((5 7) 9)
; car => (5 7)
; cdr => (7 /) <- Keep in mind, a list is *not* a pair! It's: 5 -> 7 -> /.
; car => 7
(echo (car (cdr (car (cdr (cdr one))))))
(define two (list (list 7)))
; ((7))
; car => (7)
; car => 7 <- Here we avoid that issue, since we're taking car.
(echo (car (car two)))
(define three (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (1 (2 (3 (4 (5 (6 7))))))
; cdr => ((2 (3 (4 (5 (6 7))))))
; car => (2 (3 (4 (5 (6 7))))
; cdr => ((3 (4 (5 (6 7)))))
; car => (3 (4 (5 (6 7))))
; cdr => ((4 (5 (6 7))))
; car => (4 (5 (6 7)))
; cdr => ((5 (6 7)))
; car => (5 (6 7))
; cdr => ((6 7))
; car => (6 7)
; cdr => (7 /)
; car => 7 <- 6 pairs.
(echo (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
    three)))))))))))))

; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
; (1 2 3 4 5 6)
(echo (append x y))
; ((1 2 3) 4 5 6)
(echo (cons x y))
; ((1 2 3) (4 5 6))
(echo (list x y))

; Exercise 2.27
(define (deep-reverse x)
    (define (iter y i)
        (if (null? (cdr y))
            (cons (deep-reverse (car y)) i)
            (iter (cdr y) (cons (deep-reverse (car y)) i))))
    (if (list? x)
        (iter x `())
        x))

(define x (list (list 1 2) (list 3 4) 5 6 (list (list 7 8 9))))
(echo (reverse x))
(echo (deep-reverse x))

; Exercise 2.28
; This is batshit.
; I'm trying to trade a quadratic append for-each element
; for a linear reverse at end-of-list.
; But you have no choice at a list boundary...
(define (fringe x)
    (define (iter y i)
        (cond ((null? y) i)
              ((not (list? (car y))) (iter (cdr y) (cons (car y) i)))
              (else (iter (cdr y) (append (iter (car y) `()) i)))))
    (reverse (iter x `())))

(define x (list (list 1 2) (list 3 4)))
(echo (fringe x))
(echo (fringe (list x x)))
(echo (fringe (list 1 2 (list 3 4) 5 (list (list 6 7) 8 (list 9 10 11))
                    (list 12 13))))
