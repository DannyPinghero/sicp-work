(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))


(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


; assume no malicious data input - I don't know how else to retrieve the column and row together on demand
(define (adjoin-position new k rest) (cons (list new k) rest))

(define empty-board (list))

(define (row position) (car position))
(define (col position) (cadr position))


(define (is-same? pos1 pos2) (and (= (row pos1) (row pos2)) (= (col pos1) (col pos2))))

(define (no-collision pos1 pos2)
  (

(define (safe? k all) 
  (define position (car (filter (lambda (position) (= k (col position))) all)))
  (null? (filter 
  

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(queens 4)