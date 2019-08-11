(define (echo line) (display line) (newline))

; Exercise 3.1
(define (make-accumulator sum)
    (lambda (i)  ; it's interesting that (set) doesn't take a symbol.
        (begin (set! sum (+ sum i))
               sum)))

(define X (make-accumulator 5))
(echo (X 10))  ; <-- 15
(echo (X 2))   ; <-- 17
(echo (X 15))  ; <-- 32

; Exercise 3.2
(define (make-monitored f)
    (let ((count 0))
        (lambda (arg)
            (cond ((eq? arg 'how-many-calls?) count)
                  ((eq? arg 'reset-count) (set! count 0))
                  (else (set! count (+ count 1))
                        (f arg))))))

(define s (make-monitored sqrt))
(echo (s 100))  ; <-- 10
(echo (s 9))    ; <-- 3
(echo (s 'how-many-calls?))  ; <-- 2
(s 'reset-count)
(echo (s 16))   ; <-- 4
(echo (s 'how-many-calls?))  ; <-- 1

; Exercise 3.3
; Exercise 3.4
(define (call-the-cops) "bang bang bang bang")
(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds!"))
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
    (let ((misses 0))
        (define (dispatch p m)
            (define (safe f)
                (lambda (amount)
                    (if (eq? p password)
                        (begin (set! misses 0)
                               (f amount))
                        (if (>= misses 7)
                            (call-the-cops)
                            (begin (set! misses (+ misses 1))
                                   "Incorrect password")))))
            (cond ((eq? m 'withdraw) (safe withdraw))
                  ((eq? m 'deposit) (safe deposit))
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m))))
        dispatch))

(define b (make-account 1e6 'IAmGod))
(echo ((b 'IAmGod 'deposit) 1e4))   ; <-- ok!
(echo ((b 'IAmDog 'withdraw) 1e9))  ; <-- 1.
(echo ((b 'IAmDog 'withdraw) 1e8))  ; <-- 2.
(echo ((b 'IAmGod 'deposit) 1e4))   ; <-- ok!
(echo ((b 'IAmDog 'withdraw) 1e9))  ; <-- 1.
(echo ((b 'IAmDog 'withdraw) 1e8))  ; <-- 2.
(echo ((b 'IAmDog 'withdraw) 1e9))  ; <-- 3.
(echo ((b 'IAmDog 'withdraw) 1e7))  ; <-- 4.
(echo ((b 'IAmDog 'withdraw) 1e9))  ; <-- 5.
(echo ((b 'IAmDog 'withdraw) 1e6))  ; <-- 6.
(echo ((b 'IAmDog 'withdraw) 1e9))  ; <-- 7.
(echo ((b 'IAmDog 'withdraw) 1e5))  ; <-- 8: error!

; Exercise 3.7
; The text implores us to amend our original solution.
; I think that's a mistake, honestly.
; Or, at least, I'd like not to do it.
; Two things I want to preserve with this solution:
;   1) Once a joint account has been created with a new password, the account
;      can only be accessed with the new password via the new object, not the
;      old object; e.g. (peter-acc 'rosebud 'withdraw) is an illegal access
;      because it does not go through paul-acc. This is *sort of* implied by the
;      problem statement.
;   2) All bad accesses via the new password should be included in the incorrect
;      count toward call-the-cops. So the maximum bad passwords are "shared"
;      among all the account holders. This, I'm assuming.
(define (make-joint original password alias)
    (if (eq? password alias)
        original  ; the accounts are "the same".
        (lambda (p m)
            (if (eq? p alias)
                (original password m)
                (original alias m)))))  ; guaranteed illegal dispatch,
                                        ; since alias != password.

(define peter-acc (make-account 1e4 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(echo ((peter-acc 'open-sesame 'withdraw) 100))  ; <-- 9900.
(echo ((paul-acc 'rosebud 'withdraw) 100))       ; <-- 9800.
(echo ((peter-acc 'open-sesame 'deposit) 200))   ; <-- back to 10k.
(echo ((peter-acc 'rosebud 'withdraw) 100))      ; <-- 1.
(echo ((peter-acc 'rosebud 'withdraw) 100))      ; <-- 2.
(echo ((peter-acc 'rosebud 'withdraw) 100))      ; <-- 3.
(echo ((paul-acc 'open-sesame 'withdraw) 100))   ; <-- 4.
(echo ((peter-acc 'rosebud 'withdraw) 100))      ; <-- 5.
(echo ((peter-acc 'rosebud 'withdraw) 100))      ; <-- 6.
(echo ((peter-acc 'rosebud 'withdraw) 100))      ; <-- 7.
(echo ((paul-acc 'open-sesame 'withdraw) 100))   ; <-- 8: error!

; Exercise 3.8
(define (make-f)
    (let ((chaos 1))
        (lambda (n)
            (set! chaos (* chaos n))
            chaos)))
(let ((f (make-f))
      (g (make-f)))
    (echo (+ (f 0) (f 1)))
    (echo (+ (g 1) (g 0))))
