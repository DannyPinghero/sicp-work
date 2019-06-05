(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
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
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (merge-help leaf-set tree)
  ;(display tree)
  ;(newline)
  ;(display leaf-set)
  ;(newline)
  (if (null? leaf-set)
      tree
      (if (null? tree)
          (merge-help (cddr leaf-set) (make-code-tree (car leaf-set) (cadr leaf-set)))
          (merge-help (cdr leaf-set) (make-code-tree (car leaf-set) tree)))))
      
(define (successive-merge leaf-set)
  (merge-help leaf-set (list)))

;(display (cddr (make-leaf-set (list (list 'D 1) (list 'A 4) (list 'C 1) (list 'B 2)))))
                                                                  
(newline)
(newline)
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(display (generate-huffman-tree (list (list 'D 1) (list 'A 4) (list 'C 1) (list 'B 2))))
(newline)
(display sample-tree)