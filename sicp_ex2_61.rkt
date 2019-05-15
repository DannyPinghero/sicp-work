(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(define (adjoin-set x set)
  (cond ((null? set) list(x))
        ((element-of-set? x set) set) ;it's bad that we repeat this so much, should create a helper that assumes that x is not an element of the set
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(adjoin-set 5 '(1 3 6 8 9))
