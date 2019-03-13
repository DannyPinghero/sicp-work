(define (echo line) (display line) (newline))

; Exercise 2.30
(define (square x) (* x x))
(define (square-tree tree)
    (cond ((null? tree) `())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                              (square-tree (cdr tree))))))
(define a-tree
    (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
(echo (square-tree a-tree))
(define (square-tree tree)
    (map (lambda (subtree)
            (if (pair? subtree)
                (square-tree subtree)
                (square subtree)))
         tree))
(echo (square-tree a-tree))

; Exercise 2.31
(define (tree-map f tree)
    (map (lambda (subtree)
            (if (pair? subtree)
                (tree-map f subtree)
                (f subtree)))
         tree))
(echo (tree-map square a-tree))

; Exercise 2.32
(define (subsets s)
    (if (null? s)
        (list `())
        (let ((rest (subsets (cdr s))))
            (define prepend (lambda (x) (cons (car s) x)))
            (append rest (map prepend rest)))))
; subsets(1 2 3)
;   => 1 + subsets(2 3)  <== a + [B]: [B] + [(a . b) for b in B]
;   => 1 + 2 + subsets(3)
;   => 1 + 2 + 3 + subsets()
;   => 1 + 2 + 3 + (())
;   => 1 + 2 + (() (3))
;   => 1 + (() (3) (2) (2 3))
;   => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(echo (subsets (list 1 2 3)))

