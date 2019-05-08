(define (my-equal? first second)
  (if (not (pair? first)) 
      (eq? first second)
      (and (my-equal? (car first) (car second)) (my-equal? (cdr first) (cdr second)))))


(my-equal? '(this is a list) '(this is a list))

(my-equal? '(this is a list) '(this (is a) list))