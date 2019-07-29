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
