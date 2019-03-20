(define (echo line) (display line) (newline))

; Exercise 2.33
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(echo (accumulate + 0 (list 1 2 3)))
(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) `() sequence))
(define (append seq1 seq2)  ; Oh wow...
    (accumulate cons seq2 seq1))
(define (length sequence)  ; Ha, this one took a minute...
    (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(echo (map (lambda (x) (* x 2)) (list 1 2 3)))
(echo (append (list 1 2 3) (list 4 5 6)))
(echo (length (list 1 2 3 4)))

; Exercise 2.34
; This one's a tiny bit tricky;
; You can't multiply a0 by r!
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
                0
                coefficient-sequence))
; 1 + 3x + 5x^3 + x^5
(define x 2)
(echo (horner-eval x (list 1 3 0 5 0 1)))
(echo (+ 1
         (* 3 x)
         (* 5 x x x)
         (* x x x x x)))

; Exercise 2.35
; This is actually pretty clever!
; Consider that we have a function which enumerates the leaves of a tree.
; (I produced my own version of this called "fringe".) From the book:
(define (enumerate-tree tree)
    (cond ((null? tree) `())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))
; Now: every element of a tree is a tree!
; So we can get the leaves of each "subtree", and then take the length:
(define (count-leaves t)
    (accumulate +
                0
                (map length
                     (map enumerate-tree t))))
; That is:
;   (1 (2 3 (4 5) 6) 7 8 (9 10))
;     => ( (1) (2 3 4 5 6) (7) (8) (9 10) )
;     => (+ 1       5       1   1    2    )
;     => 10
(define T (list 1 (list 2 3 (list 4 5) 6) 7 8 (list 9 10)))
(echo (map enumerate-tree T))
(echo (count-leaves T))
