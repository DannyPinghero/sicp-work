(define (echo line) (display line) (newline))

(define (make-leaf symbol weight)
    (list 'leaf symbol weight))
(define (leaf? object)
    (eq? (car object) 'leaf))
(define (symbol-leaf leaf)
    (cadr leaf))
(define (weight-leaf leaf)
    (caddr leaf))

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))
(define (make-code-tree left right)
    (list left
          right
          (append (symbols left)
                  (symbols right))
          (+ (weight left)
             (weight right))))
(define (left-branch tree)
    (car tree))
(define (right-branch tree)
    (cadr tree))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (define (choose-branch bit branch)
            (cond ((= bit 0) (left-branch branch))
                  ((= bit 1) (right-branch branch))
                  (else (error "bad bit -- CHOOSE-BRANCH" bit))))
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                 (if (leaf? next-branch)
                     (cons (symbol-leaf next-branch)
                           (decode-1 (cdr bits) tree))
                     (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
             (adjoin-set (make-leaf (car pair) (cadr pair))
                         (make-leaf-set (cdr pairs))))))

; Exercise 2.67
; Exercise 2.68
; It's unfortunate that we're not holding the symbols in ordered lists.
; Then, we could do a simple O(1) check like:
;       E
;      / \
;     /   \
;  [A  B]  \
;        [C D E] <-- E > C?
; But since we're not, we have to check every element.
; Whatever: we clearly don't care about performance here.
; By the same token, we won't concern ourselves with appending the bits.
(define (in-set? x set)
    (cond ((null? set) #f)
          ((eq? x (car set)) #t)
          (else (in-set? x (cdr set)))))
(define (encode message tree)
    (define (encode-symbol char tree)
        (define (make-bits bits node)
            (if (leaf? node)
                (if (eq? (symbol-leaf node) char)
                    bits
                    (error "not in tree:" char))
                (let ((right (right-branch node))
                      (left  (left-branch node)))
                     (if (in-set? char (symbols left))
                         (make-bits (append bits 0) left)
                         (make-bits (append bits 1) right)))))
        (make-bits '() tree))
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))
(define SAMPLE (make-code-tree (make-leaf 'A 4)
               (make-code-tree (make-leaf 'B 2)
                               (make-code-tree (make-leaf 'D 1)
                                               (make-leaf 'C 1)))))
(echo (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0)SAMPLE))  ; => A D A B B C A
(echo (encode (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0)
                      SAMPLE)
              SAMPLE))  ; => 0 1 1 0 0 1 0 1 0 1 1 1 0
