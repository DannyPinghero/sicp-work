(define (make-monitored func)
  (let ((calls 0))
    (define (how-many-calls?) calls)
    (define (reset-count)
      (begin (set! calls 0)
      calls))
    (define (call-the-thing param)
      (begin (set! calls (+ calls 1))
            (func param)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (call-the-thing m))))
    dispatch))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)

(s 25)

(s 'how-many-calls?)

(s 'reset-count)

(s 'how-many-calls?)
