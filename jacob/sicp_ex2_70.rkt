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
(define (encode-symbol symbol tree)
  (define (help symbol tree code)
    (if (leaf? tree)
        code
        (cond 
         ((memv symbol (symbols (right-branch tree))) (help symbol (right-branch tree) (append code `(1))))
         ((memv symbol (symbols (left-branch tree))) (help symbol (left-branch tree) (append code `(0))))
         
         (else (error "not in tree my dude")))))
  (help symbol tree (list)))

            
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree)))) 

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(length (encode
(list 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'SHA 'BOOM)
(generate-huffman-tree (list (list 'A 2) (list 'NA 16) (list 'BOOM 1) (list 'SHA 3) (list 'GET 2) (list 'YIP 9) (list 'JOB 2) (list 'WAH 1)))))
;87
;(1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 1)


(length (list 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'SHA 'BOOM))
;36
;36 * 3 = 108
