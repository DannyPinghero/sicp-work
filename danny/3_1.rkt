#!/usr/bin/racket
#lang racket

; 3.1

(define (make-accumulator total)
	(define (inc val)
		(set! total (+ total val))
		total
	)
	inc)

; 3.2

(define (make-monitored f)
  (define num-calls 0)
  (define (mf f-arg)
    (cond 
      ((eq? f-arg 'how-many-calls?) num-calls)
      ((eq? f-arg 'reset-count) (set! num-calls 0))
      (else (begin
              (set! num-calls (+ num-calls 1))
              (f f-arg)))))
  mf)

(define (sample-func x) (+ 1 x))



; original make account
; (define (make-account balance)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (define (dispatch m)
;     (cond ((eq? m 'withdraw) withdraw)
;           ((eq? m 'deposit) deposit)
;           (else (error "Unknown request -- MAKE-ACCOUNT"
;                        m))))
;   dispatch)
; 3.3

(define (make-account balance . passwords)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (get-balance) balance)
  
  (define (dispatch remaining-passwords password-attempt m)
    (cond ((= (length remaining-passwords) 0)
          	(lambda (x) "Incorrect Password"))
           ((eq? (car remaining-passwords) password-attempt)
			(cond ((eq? m 'withdraw) withdraw)
         	((eq? m 'deposit) deposit)
          	((eq? m 'get-balance) get-balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
           (else (dispatch (cdr remaining-passwords) password-attempt m))))
  (lambda (p m) (dispatch passwords p m)))

; 3.4
(define (make-account-2 balance password)

  (define bad-password-attempts 0)
  (define max-bad-password-attempts 7)
  (define (call-the-cops) "COPS ARE COMIN'")
  (define passwords (cons password '()))

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (add-password new-password)
    (set! passwords (cons new-password passwords))
    (display passwords)
    )
  (define (password-ok password-attempt passwords-to-try)
    (if (eq? password-attempt (car passwords-to-try))
        #t
        (password-ok password-attempt (cdr passwords-to-try))))
  (define (dispatch password-attempt m)
    (if (password-ok password-attempt passwords)
        (cond ((eq? m 'withdraw) (begin (set! bad-password-attempts 0) withdraw))
              ((eq? m 'deposit) (begin (set! bad-password-attempts 0) deposit))
              ((eq? m 'add-password) (begin (set! bad-password-attempts 0) add-password))
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) 
          (begin
            (set! bad-password-attempts (+ 1 bad-password-attempts))
            (if (> bad-password-attempts max-bad-password-attempts)
                (call-the-cops)
                "Incorrect Password")))))
  dispatch)


; 3.5

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (point-in-circle)
    
    (let ((x-coord (random-in-range x1 x2))
          (y-coord (random-in-range y1 y2)))
      (P x-coord y-coord)))
  (define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
      (cond ((= trials-remaining 0)
             (/ trials-passed trials))
            ((experiment)
             (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else
              (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))
  (let ((passed (monte-carlo trials point-in-circle))
        (rect-area (* (- y2 y1) (- x2 x1))))
    (* passed rect-area)))

(define (unit-circle x y)
  (< (+ (* x x) (* y y)) 1))


; running this 5 times with 10 trials gives an average of 3.12, not bad!
; with 100 trials: 3.136, also not bad but not a lot better
; with a million: 3.1404312

; 3.6
; based on footnote 6, a baby prng is x' = (ax + b) mod m  (linear congruential generator)
; https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use

; im going to use small ones tho

(define (rand-update x)
  (define b 10)
  (define m 17)
  (define a 30)
  (modulo (+ (* a x) b) m))
  
(define (get-rand)
  (define x 0)
  (define (inner action)
    (cond ((eq? action 'generate)
           (begin (set! x (rand-update x)) x))
          ((eq? action 'reset)
           (lambda (new-val)
             (set! x new-val)
             (set! x (rand-update x))
             x))))
  inner)

; 3.7
(define (make-joint-account acc cur-password new-password)
  ((acc cur-password 'add-password) new-password))

(define (tricky arg)
  ; this function will return what its given the first time
  ; and 0 otherwise
  (define times-called 0)
  (define (f_ arg2)
    (set! times-called (+ 1 times-called))
    (if (= times-called 1) arg2 0))
  f_)

(define f (tricky (lambda x x)))
(+ (f 0) (f 1)) ; prints 0
(define f2 (tricky (lambda x x)))
(+ (f2 1) (f2 0)) ; prints 1

