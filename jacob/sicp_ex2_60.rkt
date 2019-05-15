(define (element-of-dupe-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-dupe-set? x (cdr set)))))


(define (adjoin-dupe-set x set)
  (cons x set))

(define (intersection-dupe-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-dupe-set? (car set1) set2)        
         (cons (car set1)
               (intersection-dupe-set (cdr set1) set2)))
        (else (intersection-dupe-set (cdr set1) set2))))

; Could also do this with an append 
(define (union-dupe-set set1 set2)
  (if (null? set1)
      set2
      (union-dupe-set (cdr set1) (adjoin-dupe-set (car set1) set2))))