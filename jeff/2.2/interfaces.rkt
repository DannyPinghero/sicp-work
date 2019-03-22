(define (echo line) (display line) (newline))

; Exercise 2.33
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))
(echo (accumulate + 0 (list 1 2 3)))
; Some of these are super important.
; I'm going to override them in a function rather than global scope.
(define (overrides)
    (define (map p sequence)
        (accumulate (lambda (x y) (cons (p x) y)) `() sequence))
    (define (append seq1 seq2)  ; Oh wow...
        (accumulate cons seq2 seq1))
    (define (length sequence)  ; Ha, this one took a minute...
        (accumulate (lambda (x y) (+ y 1)) 0 sequence))
    (echo (map (lambda (x) (* x 2)) (list 1 2 3)))
    (echo (append (list 1 2 3) (list 4 5 6)))
    (echo (length (list 1 2 3 4))))
(overrides)

; Exercise 2.34
; This one's a tiny bit tricky.
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

; Exercise 2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        `()  ; This is fucking amazing.
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))
(echo (accumulate-n + 0 (list (list 1 2 3)  ; ((1 2 3) (4 5 6) (7 8 9) (10 11 12))
                              (list 4 5 6)  ; 22 -> ((2 3) (5 6) (8 9) (11 12))
                              (list 7 8 9)  ; 22 -> 26 -> ((3) (6) (9) (12))
                              (list 10 11 12))))  ; => 22 -> 26 -> 30

; Exercise 2.37
; Why is this image of such remarkably low quality?
; I can seriously barely read it.
(define (dot-product v w)
    (accumulate + 0 (map * v w)))  ; These are vectors, not matrices.
(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product v row)) m))
(define (transpose m)  ; Wow...
    (accumulate-n cons `() m))
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row) (matrix-*-vector cols row)) m)))
; Consider:
;   +---+---+---+   +----+----+   +------------------+-------------------+
;   | 1 | 2 | 3 |   | 7  | 8  |   | 1*7 + 2*9 + 3*11 | 1*8 + 2*10 + 3*12 |
;   +---+---+---+ * +----+----+ = +------------------+-------------------+
;   | 4 | 5 | 6 |   | 9  | 10 |   | 4*7 + 5*9 + 6*11 | 4*8 + 5*10 + 6*12 |
;   +---+---+---+   +----+----+   +------------------+-------------------+
;     M             | 11 | 12 |     M*N
;                   +----+----+        +------------------+-------------------+
;                     N                | (1 2 3).(7 9 11) | (1 2 3).(8 10 12) |
;                                   => +------------------+-------------------+
;                                      | (4 5 6).(7 9 11) | (4 5 6).(8 10 12) |
;                                      +------------------+-------------------+
;
;                                      +----------+----------+
;                                      | M1 . N'1 | M1 . N'2 |
;                                   => +----------+----------+
;                                      | M2 . N'1 | M2 . N'2 |
;                                      +----------+----------+
;
;                                      +---------+
;                                      | M1 * N' |
;                                   => +---------+
;                                      | M2 * N' |
;                                      +---------+
(define M (list (list 1 2 3)
                (list 4 5 6)))
(define N (list (list 7 8)
                (list 9 10)
                (list 11 12)))
(echo (matrix-*-matrix M N))
(echo (list (list (+ (* 1 7) (* 2 9)  (* 3 11))
                  (+ (* 1 8) (* 2 10) (* 3 12)))
            (list (+ (* 4 7) (* 5 9)  (* 6 11))
                  (+ (* 4 8) (* 5 10) (* 6 12)))))

; Exercise 2.38
; This is subtle!
; Notice that accumulate actually processes the list right-to-left, recursively.
; In the iterative style, we process elements as we encounter them.
(define fold-right accumulate)
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                  (cdr rest))))
    (iter initial sequence))
(echo (fold-right / 1 (list 1 2 3)))  ; 3 / 1
                                      ;   -> 2 / (3 / 1)
                                      ;   -> 1 / (2 / (3 / 1))
                                      ;   => 3 / 2
(echo (fold-left / 1 (list 1 2 3)))   ; 1 / 1
                                      ;   -> (1 / 1) / 2
                                      ;   -> ((1 / 1) / 2) / 3
                                      ;   => 1 / 6
(echo (fold-right list `() (list 1 2 3)))  ; (3 `())
                                           ;   -> (2 (3 `()))
                                           ;   -> (1 (2 (3 `())))
(echo (fold-left list `() (list 1 2 3)))   ; (`() 1)
                                           ;   -> ((`() 1) 2)
                                           ;   -> (((`() 1) 2) 3)
; Above, we have the same operands: different order, different groupings.
; So it looks like operations have to be commutative and associative.
; Which, / and list are definitely *not*!

; Exercise 2.39
(define X (list 1 2 3 4 5))
(define (reverse sequence)
    (fold-right (lambda (x y) (append y (list x))) `() sequence))
(echo (reverse X))
(define (reverse sequence)
    (fold-left (lambda (x y) (cons y x)) `() sequence))
(echo (reverse X))

; Exercise 2.40
; I don't really understand this exercise;
; the pairs in the text are already unique!
; Is it even possible to improve on what they've presented?
; I don't think so. So here it is:
(define (flatmap proc seq)
    (accumulate append `() (map proc seq)))
(define (unique-pairs n)
    (flatmap
        (lambda (i)
            (map (lambda (j) (list i j))
                 (range 1 i)))
        (range 1 n)))
(echo (unique-pairs 5))
(echo (unique-pairs 6))

; Exercise 2.41
; I'm tempted to use unique-pairs here, but I'm not sure it works.
; The requirement is that [m < n for m in (i, j, k)].
; But unique-pairs won't return, say (4, 4, 4).
; Anyway, I'm pretty satisfied with this code!
; It's a neat convergence of recursion + flatmap, at least.
; And -- bonus! -- it can handle any "arity".
; But I don't know if I find it elegant to nest scope in a flatmap lambda...
; Something about it seems a bit desperate to me; even defective?
; As if, in the absence of loops, we've cobbled something together.
(define (nary-sum arity less-than sum)
    (if (= arity 1)
        (map list
             (filter (lambda (n) (= n sum))
                     (range 1 less-than)))
        (flatmap
            (lambda (i)
                (map (lambda (j) (cons i j))
                     (nary-sum (- arity 1)
                               less-than
                               (- sum i))))
            (range 1 less-than))))
(define (triples-of at-most sum-to)
    (nary-sum 3 (+ at-most 1) sum-to))
(echo (triples-of 5 10))
(echo (triples-of 2 4))
(echo (triples-of 4 12))  ; Degenerate: ((4 4 4)).

