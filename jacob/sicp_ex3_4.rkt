(define (make-account balance password)
  (let ((calls 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (display "you're going to prison, fucko"))
    (define (dispatch pw m)
      (if (not (eq? pw password))
          (if (>= calls 2)
              (call-the-cops)
              (lambda m 
                (display "Incorrect Password")
                (begin (set! calls (+ calls 1)))))
          (cond 
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m)))))
    dispatch))


(define acc (make-account 100 'secret-password))


((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

((acc 'some-other-password 'deposit) 50)

((acc 'some-other-password 'deposit) 50)