(define (make-simple-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond 
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (lambda m 
         (display "Unknown request -- MAKE-SIMPLE-ACCOUNT" m)))))
  dispatch)


(define (make-account balance password)
  (define account (make-simple-account balance))
  (define (get-account)
      account)
  (define (set-account new-account)
      (begin (set! account new-account)))
  (define (dispatch pw m)
    (if (not (eq? pw password))
      (lambda m (display "Incorrect Password"))
 (cond
   ((eq? m 'get-account) (get-account))
((eq? m 'set-account) set-account)
(else (account m)))))
  dispatch)


(define (make-joint existing orig-pw new-pw)
  (define new-account (make-account 0 new-pw))
  ((new-account new-pw 'set-account) (existing orig-pw 'get-account))
  new-account)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 40)
((peter-acc 'open-sesame 'deposit) 20)