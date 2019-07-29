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

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password-attempt m)
    (if (eq? password-attempt password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) "Incorrect Password")))
  dispatch)


; 3.4
(define (make-account-2 balance password)

  (define bad-password-attempts 0)
  (define max-bad-password-attempts 7)
  (define (call-the-cops) "COPS ARE COMIN'")

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password-attempt m)
    (if (eq? password-attempt password)
        (cond ((eq? m 'withdraw) (begin (set! bad-password-attempts 0) withdraw))
              ((eq? m 'deposit) (begin (set! bad-password-attempts 0) deposit))
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) 
          (begin
            (set! bad-password-attempts (+ 1 bad-password-attempts))
            (if (> bad-password-attempts max-bad-password-attempts)
                (call-the-cops)
                "Incorrect Password")))))
  dispatch)


